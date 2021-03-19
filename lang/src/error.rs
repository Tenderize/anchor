use std::ops::Range;

use solana_program::program_error::ProgramError;

pub trait AnchorError: Into<ProgramError> {
    fn custom_code_range() -> Option<Range<u32>>;

    fn has_custom_code_duplication() -> bool;
}

impl AnchorError for ProgramError {
    fn custom_code_range() -> Option<Range<u32>> {
        None
    }

    fn has_custom_code_duplication() -> bool {
        false
    }
}
