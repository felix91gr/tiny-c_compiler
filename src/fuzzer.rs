use parser::parse_tc_file;
use synfuzz::char_range;
use synfuzz::many;
use synfuzz::string;
use synfuzz::Rules;

use std::collections::HashMap;
use std::sync::Arc;
use std::sync::RwLock;

use synfuzz::ch;
use synfuzz::choice;
use synfuzz::join_with;
use synfuzz::many1;
use synfuzz::register_rule;
use synfuzz::rule;
use synfuzz::seq;
use synfuzz::Generator;

use thousands::Separable;

use time::{Duration, PreciseTime};

fn ascii_alpha() -> impl Generator {
    choice!(char_range('a', 'z'), char_range('A', 'Z'))
}

fn ascii_alphanumeric() -> impl Generator {
    choice!(
        char_range('a', 'z'),
        char_range('A', 'Z'),
        char_range('0', '9')
    )
}

fn whitespace() -> impl Generator {
    many1(choice!(ch(' '), ch('\n'), ch(' '), ch(' ')))
}

fn just_space() -> impl Generator {
    ch(' ')
}

type NameAndWeight = (String, u8);

fn choice_weighted_multi(
    rule_index: Arc<RwLock<Rules>>,
    names_and_weights: Vec<NameAndWeight>,
) -> impl Generator {
    let mut rules_in_generator: Vec<Box<Generator>> = Vec::new();

    for (name, weight) in names_and_weights.iter() {
        for _i in 0..*weight {
            rules_in_generator.push(Box::new(rule(name.clone(), rule_index.clone())));
        }
    }

    choice(rules_in_generator)
}

#[test]
#[ignore]
fn fuzz_parser() {
    let recursive_rules = Arc::new(RwLock::new(HashMap::new()));

    let ids = char_range('a', 'z');
    register_rule(&recursive_rules, "ids", ids);

    let ints = {
        let int_many_digits = seq!(char_range('1', '9'), many(char_range('0', '9')));
        let int_zero = ch('0');
        choice!(int_zero, int_many_digits)
    };

    register_rule(&recursive_rules, "ints", ints);

    let _comparators = choice!(
        string("<"),
        string("<="),
        string(">="),
        string(">"),
        string("==")
    );

    let _operators = choice!(ch('+'), ch('-'));

    let term = choice_weighted_multi(
        recursive_rules.clone(),
        vec![
            ("paren_expr".to_string(), 1),
            ("ids".to_string(), 4),
            ("ints".to_string(), 4),
        ],
    );

    register_rule(&recursive_rules, "term", term);

    // Summation and Substraction
    let operation = join_with!(
        just_space(),
        rule("term", recursive_rules.clone()),
        _operators,
        rule("sum", recursive_rules.clone())
    );

    register_rule(&recursive_rules, "operation", operation);

    let sum = choice_weighted_multi(
        recursive_rules.clone(),
        vec![("operation".to_string(), 1), ("term".to_string(), 4)],
    );

    register_rule(&recursive_rules, "sum", sum);

    let comparison = join_with!(
        just_space(),
        rule("sum", recursive_rules.clone()),
        _comparators,
        rule("sum", recursive_rules.clone())
    );

    register_rule(&recursive_rules, "comparison", comparison);

    let assignment = join_with!(
        just_space(),
        rule("ids", recursive_rules.clone()),
        ch('='),
        rule("expr", recursive_rules.clone())
    );

    register_rule(&recursive_rules, "assignment", assignment);

    let expr = choice_weighted_multi(
        recursive_rules.clone(),
        vec![
            ("assignment".to_string(), 1),
            ("comparison".to_string(), 4),
            ("sum".to_string(), 4),
        ],
    );

    register_rule(&recursive_rules, "expr", expr);

    let paren_expr = join_with!(
        just_space(),
        ch('('),
        rule("expr", recursive_rules.clone()),
        ch(')')
    );

    register_rule(&recursive_rules, "paren_expr", paren_expr);

    let function_name = {
        let alphas = ascii_alpha();
        let alphanumeric = ascii_alphanumeric();

        seq!(alphas, many(alphanumeric))
    };

    register_rule(&recursive_rules, "function_name", function_name);

    let id_parameter_list = join_with!(
        just_space(),
        rule("ids", recursive_rules.clone()),
        many(join_with!(
            just_space(),
            ch(','),
            rule("ids", recursive_rules.clone())
        ))
    );
    let free_parameter_list = join_with!(
        just_space(),
        rule("expr", recursive_rules.clone()),
        many(join_with!(
            just_space(),
            ch(','),
            rule("expr", recursive_rules.clone())
        ))
    );

    register_rule(&recursive_rules, "id_parameter_list", id_parameter_list);
    register_rule(&recursive_rules, "free_parameter_list", free_parameter_list);

    let function_declaration_statement = join_with!(
        whitespace(),
        string("fn"),
        rule("function_name", recursive_rules.clone()),
        ch('('),
        rule("id_parameter_list", recursive_rules.clone()),
        ch(')'),
        rule("statement", recursive_rules.clone())
    );

    let function_usage_statement = join_with!(
        just_space(),
        string("call"),
        rule("function_name", recursive_rules.clone()),
        ch('('),
        rule("free_parameter_list", recursive_rules.clone()),
        ch(')'),
        ch(';')
    );

    register_rule(
        &recursive_rules,
        "function_declaration_statement",
        function_declaration_statement,
    );
    register_rule(
        &recursive_rules,
        "function_usage_statement",
        function_usage_statement,
    );

    let if_else_rule = join_with!(
        whitespace(),
        string("if"),
        rule("paren_expr", recursive_rules.clone()),
        rule("statement", recursive_rules.clone()),
        string("else"),
        rule("statement", recursive_rules.clone())
    );

    let if_rule = join_with!(
        whitespace(),
        string("if"),
        rule("paren_expr", recursive_rules.clone()),
        rule("statement", recursive_rules.clone())
    );

    let do_while_rule = join_with!(
        whitespace(),
        string("do"),
        rule("statement", recursive_rules.clone()),
        string("while"),
        rule("paren_expr", recursive_rules.clone()),
        ch(';')
    );

    let while_rule = join_with!(
        whitespace(),
        string("while"),
        rule("paren_expr", recursive_rules.clone()),
        rule("statement", recursive_rules.clone())
    );

    register_rule(&recursive_rules, "if_else_rule", if_else_rule);
    register_rule(&recursive_rules, "if_rule", if_rule);
    register_rule(&recursive_rules, "do_while_rule", do_while_rule);
    register_rule(&recursive_rules, "while_rule", while_rule);

    let scoped_statement = join_with!(
        whitespace(),
        ch('{'),
        many1(rule("statement", recursive_rules.clone())),
        ch('}')
    );

    register_rule(&recursive_rules, "scoped_statement", scoped_statement);

    let expr_statement = seq!(rule("expr", recursive_rules.clone()), ch(';'));

    register_rule(&recursive_rules, "expr_statement", expr_statement);

    let semicolon_statement = ch(';');

    register_rule(&recursive_rules, "semicolon_statement", semicolon_statement);

    let statement = choice_weighted_multi(
        recursive_rules.clone(),
        vec![
            ("if_else_rule".to_string(), 5),
            ("if_rule".to_string(), 10),
            ("do_while_rule".to_string(), 10),
            ("while_rule".to_string(), 10),
            ("scoped_statement".to_string(), 3),
            ("expr_statement".to_string(), 10),
            ("semicolon_statement".to_string(), 20),
            ("function_usage_statement".to_string(), 30),
            ("function_declaration_statement".to_string(), 30),
        ],
    );

    register_rule(&recursive_rules, "statement", statement);

    let mut largest_seq = 0;
    let mut largest_input = Vec::new();
    let mut well_formed = 0;

    let final_rule = rule("statement", recursive_rules.clone());

    let num_iterations = 5_000_000;

    println!(
        "Fuzzing the parser with {} iterations",
        num_iterations.separate_with_commas()
    );

    let mut time_generating = Duration::zero();
    let mut time_parsing = Duration::zero();

    for _i in 0..num_iterations {
        let started_generating = PreciseTime::now();

        let out = final_rule.generate();

        let started_parsing = PreciseTime::now();

        match parse_tc_file(&String::from_utf8_lossy(&out)) {
            Ok(_) => {
                well_formed += 1;

                let len = out.len();
                if len > largest_seq {
                    largest_seq = len;
                    largest_input = out.clone();
                }
            }
            Err(_) => {}
        };

        let done_parsing = PreciseTime::now();

        time_generating = time_generating + started_generating.to(started_parsing);
        time_parsing = time_parsing + started_parsing.to(done_parsing);

        let percentage_of_total = 100.0 * (_i as f32) / (num_iterations as f32);

        print!("Status: {:.2}% Complete         \r", percentage_of_total);
    }

    println!("Status: 100% Complete! ^u^          ");

    let correct_input_percentage: f32 = 100.0 * (well_formed as f32) / (num_iterations as f32);

    println!("Report of fuzzing:");

    println!(
        "  1) Number of correctly generated inputs: {} ({:.2}%)",
        well_formed.separate_with_commas(),
        correct_input_percentage
    );
    println!("  2) Length of largest correct input: {}", largest_seq);
    println!("  3) Largest correct input:");

    let border_line = {
        let _s = String::from_utf8_lossy(&largest_input);

        let length_of_borders = {
            let lines_of_input = _s.lines();
            lines_of_input.map(|s| s.len()).max().unwrap()
        };

        let mut total = "".into();

        for _c in 0..length_of_borders {
            total = format!("{}{}", total, "~");
        }

        total
    };

    println!("{}", border_line);
    println!("{}", &String::from_utf8_lossy(&largest_input));
    println!("{}", border_line);

    let ms_generating = time_generating.num_milliseconds() % 1000;
    let s_generating = time_generating.num_seconds() % 60;
    let m_generating = time_generating.num_minutes();

    println!(
        "  4) Time spent generating input: {:02}:{:02}.{}",
        m_generating, s_generating, ms_generating
    );

    let ms_parsing = time_parsing.num_milliseconds() % 1000;
    let s_parsing = time_parsing.num_seconds() % 60;
    let m_parsing = time_parsing.num_minutes();

    println!(
        "  5) Time spent parsing input: {:02}:{:02}.{}",
        m_parsing, s_parsing, ms_parsing
    );
}
