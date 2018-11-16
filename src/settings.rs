//////////////////////////////////////
//          Global Settings         //
//////////////////////////////////////

pub static mut VERBOSITY: u64 = 0;

pub fn verbosity() -> u64 {
	unsafe {
		VERBOSITY
	}
}

#[macro_export]
macro_rules! vprintln {
    ($($arg:tt)*) => ({
      if verbosity() > 0 {
        println!($($arg)*);
      }
    })
}
