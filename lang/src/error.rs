use solana_program::program_error::ProgramError;
use std::collections::HashSet;

pub trait AnchorError: Into<ProgramError> {
    fn used_error_codes() -> HashSet<u32>;
}
