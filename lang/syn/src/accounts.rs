pub mod constraints;
pub mod field;

#[cfg(feature = "idl")]
use crate::idl::{IdlAccount, IdlAccountItem, IdlAccounts};
use field::{AccountFieldGenerator, MetaAccountFieldGenerator, Ty};
use heck::{MixedCase, SnakeCase};
use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use std::{collections::HashMap, convert::TryFrom};
use syn::{
    parse::{Parse, ParseStream},
    Attribute, Error, Field, Fields, FieldsUnnamed, Ident, ItemStruct, Result,
};

use self::field::AccountField;

#[derive(Debug)]
pub struct AccountsStruct {
    // Name of the accounts struct.
    pub ident: syn::Ident,
    // Generics + lifetimes on the accounts struct.
    pub generics: syn::Generics,
    // Fields on the accounts struct.
    pub fields: Vec<AccountField>,
}

impl AccountsStruct {
    pub fn new(strct: syn::ItemStruct, fields: Vec<AccountField>) -> Self {
        let ident = strct.ident.clone();
        let generics = strct.generics;
        Self {
            ident,
            generics,
            fields,
        }
    }

    // Returns all program owned accounts in the Accounts struct.
    //
    // `global_accs` is given to "link" account types that are embedded
    // in each other.
    pub fn account_tys(
        &self,
        global_accs: &HashMap<String, AccountsStruct>,
    ) -> Result<Vec<String>> {
        let mut tys = vec![];
        for f in &self.fields {
            match f {
                AccountField::PlainField(f) => {
                    if let Ty::ProgramAccount(pty) = &f.ty {
                        tys.push(pty.account_ident.to_string());
                    }
                }
                AccountField::CompositeField(comp_f) => {
                    let accs = global_accs.get(&comp_f.symbol).ok_or_else(|| {
                        Error::new_spanned(&comp_f.raw_field, "Invalid account type")
                    })?;
                    tys.extend(accs.account_tys(global_accs)?);
                }
            }
        }
        Ok(tys)
    }

    #[cfg(feature = "idl")]
    pub fn idl_accounts(
        &self,
        global_accs: &HashMap<String, AccountsStruct>,
    ) -> Vec<IdlAccountItem> {
        self.fields
            .iter()
            .map(|acc: &AccountField| match acc {
                AccountField::CompositeField(comp_f) => {
                    let accs_strct = global_accs
                        .get(&comp_f.symbol)
                        .expect("Could not reslve Accounts symbol");
                    let accounts = accs_strct.idl_accounts(global_accs);
                    IdlAccountItem::IdlAccounts(IdlAccounts {
                        name: comp_f.ident.to_string().to_mixed_case(),
                        accounts,
                    })
                }
                AccountField::PlainField(acc) => IdlAccountItem::IdlAccount(IdlAccount {
                    name: acc.ident.to_string().to_mixed_case(),
                    is_mut: acc.constraints.is_mut(),
                    is_signer: acc.constraints.is_signer(),
                }),
            })
            .collect::<Vec<_>>()
    }

    pub fn meta_struct(&self) -> TokenStream {
        let mod_name = Ident::new(
            &format!(
                "__client_accounts_{}",
                self.ident.to_string().to_snake_case()
            ),
            Span::call_site(),
        );

        let name = Ident::new(&format!("{}Meta", self.ident), Span::call_site());

        let fields = self.fields.iter().map(MetaAccountFieldGenerator::field);

        let to_account_metas_impl = self.meta_to_account_metas_impl();

        quote! {
            mod #mod_name {
                use super::*;
                use anchor_lang::prelude::borsh;

                #[derive(anchor_lang::AnchorSerialize)]
                struct #name {
                    #(#fields)*
                }

                #to_account_metas_impl
            }

            pub use #mod_name::*;
        }
    }

    pub fn meta_to_account_metas_impl(&self) -> TokenStream {
        let name = Ident::new(&format!("{}Meta", &self.ident), Span::call_site());

        let meta_account_metas = self
            .fields
            .iter()
            .map(MetaAccountFieldGenerator::to_account_metas);

        let field_count = self.fields.len();

        quote! {
            impl anchor_lang::ToAccountMetas for #name {
                fn to_account_metas(&self, is_signer: Option<bool>) -> Vec<anchor_lang::solana_program::instruction::AccountMeta> {
                    let mut account_metas = Vec::with_capacity(#field_count);

                    use anchor_lang::ToAccountInfo;
                    #(#meta_account_metas)*

                    account_metas
                }
            }
        }
    }

    pub fn to_account_metas_impl(&self) -> TokenStream {
        let name = &self.ident;

        let (combined_generics, trait_generics, strct_generics) = match self.generics.lt_token {
            None => (quote! {<'info>}, quote! {<'info>}, quote! {}),
            Some(_) => {
                let g = &self.generics;
                (quote! {#g}, quote! {#g}, quote! {#g})
            }
        };

        let to_acc_metas = self
            .fields
            .iter()
            .map(AccountFieldGenerator::to_account_metas);

        let field_count = self.fields.len();

        quote! {
            impl#combined_generics anchor_lang::ToAccountMetas for #name#strct_generics {
                fn to_account_metas(&self, is_signer: Option<bool>) -> Vec<anchor_lang::solana_program::instruction::AccountMeta> {
                    let mut account_metas = Vec::with_capacity(#field_count);

                    use anchor_lang::ToAccountInfo;
                    #(#to_acc_metas)*

                    account_metas
                }
            }
        }
    }

    pub fn accounts_impl(&self) -> TokenStream {
        let name = &self.ident;

        let (combined_generics, trait_generics, strct_generics) = match self.generics.lt_token {
            None => (quote! {<'info>}, quote! {<'info>}, quote! {}),
            Some(_) => {
                let g = &self.generics;
                (quote! {#g}, quote! {#g}, quote! {#g})
            }
        };

        // Deserialization for each field.
        let deser_fields = self.fields.iter().map(AccountFieldGenerator::deser);

        // Constraint checks for each account fields.
        let access_checks = self.fields.iter().map(AccountFieldGenerator::access_checks);

        // Each field in the final deserialized accounts struct.
        let return_tys = self.fields.iter().map(AccountField::ident);

        quote! {
            impl#combined_generics anchor_lang::Accounts#trait_generics for #name#strct_generics {
                #[inline(never)]
                fn try_accounts(program_id: &anchor_lang::solana_program::pubkey::Pubkey, accounts: &mut &[anchor_lang::solana_program::account_info::AccountInfo<'info>]) -> std::result::Result<Self, anchor_lang::solana_program::program_error::ProgramError> {
                    // Deserialize each account.
                    #(#deser_fields)*

                    // Perform constraint checks on each account.
                    #(#access_checks)*

                    // Success. Return the validated accounts.
                    Ok(#name {
                        #(#return_tys),*
                    })
                }
            }
        }
    }

    pub fn to_account_infos_impl(&self) -> TokenStream {
        let name = &self.ident;
        let field_count = self.fields.len();

        let (combined_generics, trait_generics, strct_generics) = match self.generics.lt_token {
            None => (quote! {<'info>}, quote! {<'info>}, quote! {}),
            Some(_) => {
                let g = &self.generics;
                (quote! {#g}, quote! {#g}, quote! {#g})
            }
        };

        let to_acc_infos = self
            .fields
            .iter()
            .map(AccountFieldGenerator::to_account_infos);

        quote! {
            impl#combined_generics anchor_lang::ToAccountInfos#trait_generics for #name#strct_generics {
                fn to_account_infos(&self) -> Vec<anchor_lang::solana_program::account_info::AccountInfo<'info>> {
                    let mut account_infos = Vec::with_capacity(#field_count);

                    use anchor_lang::ToAccountInfo;
                    #(#to_acc_infos)*

                    account_infos
                }
            }
        }
    }

    pub fn to_accounts_exit_impl(&self) -> TokenStream {
        let name = &self.ident;

        let (combined_generics, trait_generics, strct_generics) = match self.generics.lt_token {
            None => (quote! {<'info>}, quote! {<'info>}, quote! {}),
            Some(_) => {
                let g = &self.generics;
                (quote! {#g}, quote! {#g}, quote! {#g})
            }
        };

        let on_save = self.fields.iter().map(AccountField::on_save);

        quote! {
            impl#combined_generics anchor_lang::AccountsExit#trait_generics for #name#strct_generics {
                fn exit(&self, program_id: &anchor_lang::solana_program::pubkey::Pubkey) -> anchor_lang::solana_program::entrypoint::ProgramResult {
                    #(#on_save)*
                    Ok(())
                }
            }
        }
    }
}

impl Parse for AccountsStruct {
    fn parse(input: ParseStream) -> Result<Self> {
        Self::try_from(<ItemStruct as Parse>::parse(input)?)
    }
}

impl TryFrom<ItemStruct> for AccountsStruct {
    type Error = Error;

    fn try_from(input: ItemStruct) -> Result<Self> {
        let fields = match &input.fields {
            Fields::Named(fields) => fields
                .named
                .iter()
                .map(|field| AccountField::try_from(field.clone()))
                .collect::<Result<Vec<_>>>()?,
            Fields::Unnamed(FieldsUnnamed { paren_token, .. }) => {
                return Err(Error::new_spanned(
                    input.fields,
                    "Unnamed Accounts fields are not supported",
                ))
            }
            Fields::Unit => Vec::new(),
        };
        Ok(AccountsStruct::new(input.clone(), fields))
    }
}

impl From<&AccountsStruct> for TokenStream {
    fn from(accounts: &AccountsStruct) -> Self {
        let accounts_impl = accounts.accounts_impl();
        let to_account_infos_impl = accounts.to_account_infos_impl();
        let to_account_metas_impl = accounts.to_account_metas_impl();
        let to_accounts_exit_impl = accounts.to_accounts_exit_impl();

        let meta_struct = accounts.meta_struct();

        quote! {
            #accounts_impl
            #to_account_infos_impl
            #to_account_metas_impl // impl ToAccountMetas
            #to_accounts_exit_impl

            #meta_struct
        }
    }
}

impl ToTokens for AccountsStruct {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend::<TokenStream>(self.into());
    }
}

pub fn is_account_attr(attr: &&Attribute) -> bool {
    attr.path
        .get_ident()
        .map_or(false, |ident| ident == "account")
}
