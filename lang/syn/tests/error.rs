use core::panic;
use std::convert::TryFrom;

use anchor_syn::ErrorItem;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{parse2, parse_quote, DeriveInput, File, ItemEnum};

#[test]
pub fn not_attributed_error_enum() {
    let input: DeriveInput = parse_quote! {
        enum SimpleError {
            Unit,
            Named { a: u32, b: String },
            Unnamed(Option<bool>)
        }
    };
    let parsed = match ErrorItem::try_from(input).unwrap() {
        ErrorItem::Enum(parsed) => parsed,
        ErrorItem::Struct(_) => {
            panic!("It must be enum not struct");
        }
    };

    assert_eq!(&parsed.ident.to_string(), "SimpleError");
    assert!(parsed.custom_code_range.is_none());
    let mut variant_it = parsed.variants.iter();
    {
        let variant = variant_it.next().unwrap();
        assert_eq!(&variant.ident.to_string(), "Unit")
    }
    {
        let variant = variant_it.next().unwrap();
        assert_eq!(&variant.ident.to_string(), "Named")
    }
    {
        let variant = variant_it.next().unwrap();
        assert_eq!(&variant.ident.to_string(), "Unnamed")
    }

    let output = TokenStream::try_from(parsed).unwrap();
    let output = parse2::<File>(output).unwrap();

    assert_eq!(
        output,
        parse_quote! {
            #[allow(unused_qualifications)]
            impl From<SimpleError> for anchor_lang::solana_program::program_error::ProgramError {
                fn from(error: SimpleError) -> Self {
                    match error {
                        SimpleError::Unit => {
                            anchor_lang::solana_program::program_error::ProgramError::Custom(0u32)
                        }
                        SimpleError::Named { .. } => {
                            anchor_lang::solana_program::program_error::ProgramError::Custom(1u32)
                        }
                        SimpleError::Unnamed( .. ) => {
                            anchor_lang::solana_program::program_error::ProgramError::Custom(2u32)
                        }
                    }
                }
            }
        },
        "\ngenerated code was {}",
        output.to_token_stream().to_string()
    );
}

#[test]
pub fn attributed_error_enum() {
    let input: DeriveInput = parse_quote! {
        #[range(0, 100)]
        enum AttributedError {
            #[custom(23)]
            #[msg("Unit {}", "variant")]
            Unit,
            #[msg("Named {} {with} {a} {b}", "variant", with = ":")]
            Named { a: u32, b: String, #[from] simple: SimpleError },
            #[alias(InvalidAccountData)]
            #[msg("Unnamed {} {}", "variant")]
            Unnamed(bool)
        }
    };
    let parsed = match ErrorItem::try_from(input).unwrap() {
        ErrorItem::Enum(parsed) => parsed,
        ErrorItem::Struct(_) => {
            panic!("It must be enum not struct");
        }
    };

    assert_eq!(&parsed.ident.to_string(), "AttributedError");
    assert_eq!(parsed.custom_code_range, Some(0..100));

    let mut variant_it = parsed.variants.iter();
    {
        let variant = variant_it.next().unwrap();
        assert_eq!(&variant.ident.to_string(), "Unit")
    }
    {
        let variant = variant_it.next().unwrap();
        assert_eq!(&variant.ident.to_string(), "Named")
    }
    {
        let variant = variant_it.next().unwrap();
        assert_eq!(&variant.ident.to_string(), "Unnamed")
    }

    let output = TokenStream::try_from(parsed).unwrap();
    let output = parse2::<File>(output).unwrap();

    assert_eq!(
        output,
        parse_quote! {
            #[allow(unused_qualifications)]
            impl From<SimpleError> for anchor_lang::solana_program::program_error::ProgramError {
                fn from(error: SimpleError) -> Self {
                    match error {
                        SimpleError::Unit => {
                            msg!("Unit {}", "variant");
                            anchor_lang::solana_program::program_error::ProgramError::Custom(23u32)
                        }
                        SimpleError::Named { a, b, simple } => {
                            msg!("Named {} {with} {a} {b}", "variant", with = ":", a = &a, b = &b, simple = &simple);
                            simple.into()
                        }
                        SimpleError::Unnamed( _id0 ) => {
                            msg!("Unnamed {} {}", "variant", _id0);
                            anchor_lang::solana_program::program_error::ProgramError::InvalidAccountData
                        }
                    }
                }
            }
        },
        "\ngenerated code was {}",
        output.to_token_stream().to_string()
    );
}
