extern crate proc_macro;

use std::convert::{TryFrom, TryInto};

use anchor_syn::parser::error as error_parser;
use anchor_syn::{codegen::error as error_codegen, ErrorItem};
use proc_macro::TokenStream;
use quote::quote_spanned;
use syn::parse_macro_input;
use syn::{DeriveInput, Item, ItemStruct, ItemUnion};

#[proc_macro_derive(AnchorError, attributes(msg, from, custom, alias))]
pub fn derive_anchor_deserialize(item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as ErrorItem);

    proc_macro2::TokenStream::try_from(input)
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}
