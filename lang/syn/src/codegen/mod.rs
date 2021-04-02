use proc_macro2::Span;
use syn::Ident;

pub mod error;
pub mod program;

use proc_macro_crate::{crate_name, FoundCrate};

pub(crate) fn anchor_lang_crate_name() -> Ident {
    match crate_name("anchor_lang") {
        Ok(FoundCrate::Itself) => Ident::new("crate", Span::call_site()),
        Ok(FoundCrate::Name(name)) => Ident::new(&name, Span::call_site()),
        Err(_) => Ident::new("anchor_lang", Span::call_site()),
    }
}
