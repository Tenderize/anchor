use std::{
    convert::{TryFrom, TryInto},
    ops::Range,
};

use crate::{
    codegen::anchor_lang_crate_name, ErrorItem, ErrorStruct, ErrorVariant, ErrorVariantMessage,
};
use crate::{ErrorEnum, ErrorTarget};
use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{Error, FieldsNamed, FieldsUnnamed, Ident, LitInt, Result};

impl TryFrom<ErrorItem> for TokenStream {
    type Error = Error;

    fn try_from(error_item: ErrorItem) -> Result<Self> {
        match error_item {
            ErrorItem::Enum(error_enum) => error_enum.try_into(),
            ErrorItem::Struct(error_struct) => error_struct.try_into(),
        }
    }
}

impl TryFrom<ErrorStruct> for TokenStream {
    type Error = Error;

    fn try_from(error_struct: ErrorStruct) -> Result<Self> {
        panic!("TODO")
    }
}

impl TryFrom<ErrorEnum> for TokenStream {
    type Error = Error;

    fn try_from(error_enum: ErrorEnum) -> Result<Self> {
        let mut next_code = Some(error_enum.custom_code_range.as_ref().map_or(0, |r| r.start));

        let ty = &error_enum.ident;

        let anchor_lang_crate = anchor_lang_crate_name();

        let (impl_generics, ty_generics, where_clause) = error_enum.generics.split_for_impl();

        let mut body = TokenStream::new();

        for variant in error_enum.variants {
            body.extend(variant.generate(&ty, &error_enum.custom_code_range, &mut next_code)?);
        }

        Ok(quote! {
            #[allow(unused_qualifications)]
            impl #impl_generics From<#ty #ty_generics> for #anchor_lang_crate::solana_program::program_error::ProgramError #where_clause {
                fn from(error: #ty) -> Self {
                    match error {
                        #body
                    }
                }
            }

            /*#[allow(unused_qualifications)]
            impl #impl_generics #anchor_lang_crate::AnchorError for #ty #ty_generics #where_clause {
                fn used_error_codes() -> std::collections::BTreeSet<u32> {
                    let result = std::collections::BTreeSet::new();
                    #(result.insert(#used_codes);)*
                    result
                }
            }*/
        })
    }
}

impl ErrorVariant {
    pub fn generate(
        &self,
        ty: &Ident,
        custom_code_range: &Option<Range<u32>>,
        next_code: &mut Option<u32>,
    ) -> Result<TokenStream> {
        let anchor_lang_crate = anchor_lang_crate_name();

        let target = match &self.target {
            ErrorTarget::Custom(code) => {
                let code = if let Some(code_lit) = code {
                    let code = code_lit.base10_parse()?;
                    if let Some(next_code) = next_code {
                        if code < *next_code {
                            return Err(Error::new_spanned(
                                code_lit,
                                &format!("Custom code is not ordered. Must be >= {}", next_code),
                            ));
                        }
                    }
                    if let Some(code_range) = custom_code_range {
                        if !code_range.contains(&code) {
                            return Err(Error::new_spanned(
                                code_lit,
                                &format!(
                                    "Custom code is not in range. Must be < {}",
                                    code_range.end
                                ),
                            ));
                        }
                    }
                    code
                } else {
                    let code = next_code.ok_or_else(|| {
                        Error::new_spanned(self.raw.clone(), "Custom error code required")
                    })?;
                    if let Some(code_range) = custom_code_range {
                        if !code_range.contains(&code) {
                            return Err(Error::new_spanned(
                                &self.raw,
                                "Too many enum branches. Not fit in range",
                            ));
                        }
                    }
                    code
                };

                *next_code = Some(code + 1);
                quote! { #anchor_lang_crate::solana_program::program_error::ProgramError::Custom(#code) }
            }
            ErrorTarget::Alias(target) => {
                quote! { #anchor_lang_crate::solana_program::program_error::ProgramError::#target }
            }
            ErrorTarget::From { field_index } => match &self.raw.fields {
                syn::Fields::Named(FieldsNamed { named, .. }) => {
                    panic!("TODO")
                }
                syn::Fields::Unnamed(FieldsUnnamed { unnamed, .. }) => {
                    panic!("TODO")
                }
                syn::Fields::Unit => {
                    unreachable!()
                }
            },
        };

        let (mut positional_msg_args, mut named_msg_args) = if let Some(ErrorVariantMessage {
            format: _,
            positinal_args,
            named_args,
        }) = &self.message
        {
            (positinal_args.clone(), named_args.clone())
        } else {
            (TokenStream::new(), TokenStream::new())
        };

        let variant_header = match &self.fields {
            syn::Fields::Named(fields) => {
                if self.message.is_some() {
                    let mut variant_header = TokenStream::new();
                    for field in &fields.named {
                        let field_name = field.ident.as_ref().unwrap();
                        if self.message.is_some() {
                            variant_header.extend(quote! { #field_name, });
                            named_msg_args.extend(quote! { #field_name = #field_name, })
                        }
                    }
                    quote! { { #variant_header } }
                } else {
                    quote! { { .. } }
                }
            }
            syn::Fields::Unnamed(fields) => {
                if self.message.is_some() {
                    let mut variant_header = TokenStream::new();
                    for (field_idx, _field) in fields.unnamed.iter().enumerate() {
                        let field_name =
                            Ident::new(format!("_id{}", field_idx).as_str(), Span::call_site());
                        if self.message.is_some() {
                            variant_header.extend(quote! { #field_name, });
                            positional_msg_args.extend(quote! { #field_name, })
                        }
                    }
                    quote! { ( #variant_header )}
                } else {
                    quote! { ( .. )}
                }
            }
            syn::Fields::Unit => TokenStream::new(),
        };

        let variant_name = &self.ident;

        let msg = self.message.as_ref().map_or(
                TokenStream::new(),
                    |ErrorVariantMessage { format, .. }| quote! { msg!(#format, #positional_msg_args #named_msg_args); });

        Ok(quote! {
            #ty::#variant_name #variant_header => {
                #msg
                #target
            }
        })
    }
}
