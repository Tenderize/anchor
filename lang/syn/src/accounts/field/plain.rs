use std::{
    collections::BTreeSet,
    convert::{TryFrom, TryInto},
};

use super::{AccountFieldGenerator, MetaAccountFieldGenerator, SysvarTy, Ty};
use crate::{
    accounts::{
        constraints::{
            ConstraintAddress, ConstraintBelongsTo, ConstraintBump, ConstraintBumpSave,
            ConstraintExpr, ConstraintInit, ConstraintMut, ConstraintOwner, ConstraintRentExempt,
            ConstraintSeeds, ConstraintSigner, SpannedConstraint,
        },
        is_account_attr,
    },
    WithContext, WithSpan,
};
use proc_macro2::{Ident, Span, TokenStream};
use quote::{quote, quote_spanned, ToTokens};
use syn::{
    parse2, punctuated::Punctuated, spanned::Spanned, token::Comma, Attribute, Error, Field, Result,
};

#[derive(Debug)]
pub struct PlainField {
    pub ident: Ident,
    pub ty: Ty,
    pub constraints: PlainConstraints,
}

impl MetaAccountFieldGenerator for PlainField {
    fn field(&self) -> TokenStream {
        let name = &self.ident;

        quote! {
            pub #name: anchor_lang::solana_program::pubkey::Pubkey,
        }
    }

    fn to_account_metas(&self) -> TokenStream {
        let is_signer = match self.constraints.is_signer() {
            false => quote! {false},
            true => quote! {true},
        };
        let meta = match self.constraints.is_mut() {
            false => quote! { anchor_lang::solana_program::instruction::AccountMeta::new_readonly },
            true => quote! { anchor_lang::solana_program::instruction::AccountMeta::new },
        };
        let name = &self.ident;
        quote! {
            account_metas.push(#meta(self.#name, #is_signer));
        }
    }
}

impl AccountFieldGenerator for PlainField {
    fn deser(&self) -> TokenStream {
        let name = self.typed_ident();
        match self.constraints.is_init() {
            false => quote! {
                let #name = anchor_lang::Accounts::try_accounts(program_id, accounts)?;
            },
            true => quote! {
                let mut #name = anchor_lang::AccountsInit::try_accounts_init(program_id, accounts)?;
            },
        }
    }

    fn access_checks(&self) -> TokenStream {
        WithContext::new(&self.constraints, self).to_token_stream()
    }

    fn to_account_metas(&self) -> TokenStream {
        let is_signer = match self.constraints.is_signer() {
            false => quote! {false},
            true => quote! {true},
        };
        let meta = match self.constraints.is_mut() {
            false => quote! { anchor_lang::solana_program::instruction::AccountMeta::new_readonly },
            true => quote! { anchor_lang::solana_program::instruction::AccountMeta::new },
        };
        let name = &self.ident;
        quote! {
            account_metas.push(#meta(self.#name.to_account_info().key.clone(), #is_signer));
        }
    }

    fn to_account_infos(&self) -> TokenStream {
        let ident = &self.ident;
        quote! {
            account_infos.extend(self.#ident.to_account_infos());
        }
    }

    fn on_save(&self) -> TokenStream {
        let ident = &self.ident;
        if self.constraints.is_mut() {
            quote! {
                anchor_lang::AccountsExit::exit(&self.#ident, program_id)?;
            }
        } else {
            quote! {}
        }
    }
}

impl PlainField {
    pub fn typed_ident(&self) -> proc_macro2::TokenStream {
        let name = &self.ident;

        let ty = match &self.ty {
            Ty::AccountInfo => quote! { AccountInfo },
            Ty::ProgramState(ty) => {
                let account = &ty.account_ident;
                quote! {
                    ProgramState<#account>
                }
            }
            Ty::ProgramAccount(ty) => {
                let account = &ty.account_ident;
                quote! {
                    ProgramAccount<#account>
                }
            }
            Ty::CpiAccount(ty) => {
                let account = &ty.account_ident;
                quote! {
                    CpiAccount<#account>
                }
            }
            Ty::Sysvar(ty) => {
                let account = match ty {
                    SysvarTy::Clock => quote! {Clock},
                    SysvarTy::Rent => quote! {Rent},
                    SysvarTy::EpochSchedule => quote! {EpochSchedule},
                    SysvarTy::Fees => quote! {Fees},
                    SysvarTy::RecentBlockHashes => quote! {RecentBlockHashes},
                    SysvarTy::SlotHashes => quote! {SlotHashes},
                    SysvarTy::SlotHistory => quote! {SlotHistory},
                    SysvarTy::StakeHistory => quote! {StakeHistory},
                    SysvarTy::Instructions => quote! {Instructions},
                    SysvarTy::Rewards => quote! {Rewards},
                };
                quote! {
                    Sysvar<#account>
                }
            }
            Ty::ChunkAccount(ty) => {
                let item = &ty.item_ident;
                quote! {
                    CpiAccount<#item>
                }
            }
            Ty::Compound(_) => unreachable!(),
        };

        quote! {
            #name: #ty
        }
    }
}

impl TryFrom<Field> for PlainField {
    type Error = Error;

    fn try_from(field: Field) -> Result<Self> {
        let ty: Ty = field.ty.try_into()?;
        let mut constraints_builder = PlainConstraintsBuilder::new(ty.clone());
        for attr in field.attrs.iter().filter(is_account_attr) {
            constraints_builder.add_attr(attr)?;
        }

        Ok(Self {
            ident: field.ident.unwrap(),
            ty,
            constraints: constraints_builder.build()?,
        })
    }
}

#[derive(Debug)]
pub struct PlainConstraints {
    init: Option<ConstraintInit>,
    is_mut: Option<ConstraintMut>,
    signer: Option<ConstraintSigner>,
    address: Option<ConstraintAddress>,
    seeds: Option<ConstraintSeeds>,
    bump: Option<ConstraintBump>,
    bump_save: Option<ConstraintBumpSave>,
    belongs_to: BTreeSet<ConstraintBelongsTo>,
    owner: Option<ConstraintOwner>,
    rent_exempt: Option<ConstraintRentExempt>,
    exprs: Vec<ConstraintExpr>,
}

impl PlainConstraints {
    pub fn is_init(&self) -> bool {
        self.init.is_some()
    }

    pub fn is_mut(&self) -> bool {
        self.is_mut.is_some()
    }

    pub fn is_signer(&self) -> bool {
        self.signer.is_some()
    }
}

impl<'a> ToTokens for WithContext<&'a PlainConstraints, &'a PlainField> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        if let Some(init) = &self.inner.init {
            WithContext::new(init, self.context).to_tokens(tokens);
        }

        if let Some(is_mut) = &self.inner.is_mut {
            WithContext::new(is_mut, self.context).to_tokens(tokens);
        }

        if let Some(signer) = &self.inner.signer {
            WithContext::new(signer, self.context).to_tokens(tokens);
        }

        if let Some(seeds) = &self.inner.seeds {
            WithContext::new(seeds, self.context).to_tokens(tokens);
        }

        for belongs_to in &self.inner.belongs_to {
            WithContext::new(belongs_to, self.context).to_tokens(tokens);
        }

        if let Some(owner) = &self.inner.owner {
            WithContext::new(owner, self.context).to_tokens(tokens);
        }

        if let Some(rent_exempt) = &self.inner.rent_exempt {
            WithContext::new(rent_exempt, self.context).to_tokens(tokens);
        }
    }
}

#[derive(Debug)]
pub struct PlainConstraintsBuilder {
    field_ty: Ty,
    init: Option<ConstraintInit>,
    r#mut: Option<ConstraintMut>,
    signer: Option<ConstraintSigner>,
    address: Option<ConstraintAddress>,
    seeds: Option<ConstraintSeeds>,
    bump: Option<ConstraintBump>,
    bump_save: Option<ConstraintBumpSave>,
    belongs_to: BTreeSet<ConstraintBelongsTo>,
    owner: Option<ConstraintOwner>,
    rent_exempt: Option<ConstraintRentExempt>,
    exprs: Vec<ConstraintExpr>,
}

impl PlainConstraintsBuilder {
    pub fn new(field_ty: Ty) -> Self {
        PlainConstraintsBuilder {
            field_ty,
            init: None,
            r#mut: None,
            signer: None,
            address: None,
            seeds: None,
            bump: None,
            bump_save: None,
            belongs_to: BTreeSet::new(),
            owner: None,
            rent_exempt: None,
            exprs: Vec::new(),
        }
    }

    pub fn add_attr(&mut self, attr: &Attribute) -> Result<()> {
        let components =
            attr.parse_args_with(Punctuated::<SpannedConstraint, Comma>::parse_terminated)?;

        for component in components {
            self.add_constraint(component)?;
        }

        Ok(())
    }

    pub fn add_constraint(&mut self, constraint: SpannedConstraint) -> Result<()> {
        match constraint {
            SpannedConstraint::Init(init) => self.add_init(init),
            SpannedConstraint::Mut(r#mut) => self.add_mut(r#mut),
            SpannedConstraint::Signer(signer) => self.add_signer(signer),
            SpannedConstraint::Address(address) => self.add_address(address),
            SpannedConstraint::Seeds(seeds) => self.add_seeds(seeds),
            SpannedConstraint::Bump(bump) => self.add_bump(bump),
            SpannedConstraint::BumpSave(bump_save) => self.add_bump_save(bump_save),
            SpannedConstraint::BelongsTo(belongs_to) => self.add_belongs_to(belongs_to),
            SpannedConstraint::Owner(owner) => self.add_owner(owner),
            SpannedConstraint::RentExempt(rent_exempt) => self.add_rent_exempt(rent_exempt),
            SpannedConstraint::Expr(expr) => self.add_expr(expr),
        }
    }

    pub fn add_init(&mut self, init: WithSpan<ConstraintInit>) -> Result<()> {
        if self.init.is_some() {
            return Err(Error::new(init.context(), "Duplicated account::init"));
        }
        self.init.replace(init.into_inner());

        Ok(())
    }

    pub fn add_mut(&mut self, r#mut: WithSpan<ConstraintMut>) -> Result<()> {
        if self.r#mut.is_some() {
            return Err(Error::new(r#mut.context(), "Duplicated account::mut"));
        }
        self.r#mut.replace(r#mut.into_inner());

        Ok(())
    }

    pub fn add_signer(&mut self, signer: WithSpan<ConstraintSigner>) -> Result<()> {
        if self.signer.is_some() {
            return Err(Error::new(signer.span(), "Duplicated account::signer"));
        }
        match &self.field_ty {
            Ty::AccountInfo | Ty::ProgramState(_) | Ty::ProgramAccount(_) => {}
            _ => {
                return Err(Error::new(
                    signer.context(),
                    format!(
                        "account::signer is not compatible with field type {}",
                        self.field_ty
                    ),
                ))
            }
        }
        self.signer.replace(signer.into_inner());

        Ok(())
    }

    pub fn add_address(&mut self, address: WithSpan<ConstraintAddress>) -> Result<()> {
        if self.address.is_some() {
            return Err(Error::new(address.span(), "Duplicated attribute"));
        }

        if self.seeds.is_some() {
            return Err(Error::new(
                address.span(),
                "Only one of address or seeds may be used",
            ));
        }

        self.address.replace(address.into_inner());

        Ok(())
    }

    pub fn add_seeds(&mut self, seeds: WithSpan<ConstraintSeeds>) -> Result<()> {
        if self.seeds.is_some() {
            return Err(Error::new(seeds.context(), "Duplicated account::seeds"));
        }

        if self.address.is_some() {
            return Err(Error::new(
                seeds.span(),
                "Only one of address or seeds may be used",
            ));
        }

        self.seeds.replace(seeds.into_inner());

        Ok(())
    }

    pub fn add_bump(&mut self, bump: WithSpan<ConstraintBump>) -> Result<()> {
        if self.seeds.is_none() {
            return Err(Error::new(bump.context(), "must be after account::seeds"));
        }

        if self.bump_save.is_some() {
            return Err(Error::new(
                bump.context(),
                "only one of account::bump and account::bump_save may be present",
            ));
        }

        if self.bump.is_some() {
            return Err(Error::new(bump.context(), "duplicated attribute"));
        }

        self.bump.replace(bump.into_inner());
        Ok(())
    }

    pub fn add_bump_save(&mut self, bump_save: WithSpan<ConstraintBumpSave>) -> Result<()> {
        if self.seeds.is_none() {
            return Err(Error::new(
                bump_save.context(),
                "must be after account::seeds",
            ));
        }

        if self.bump.is_some() {
            return Err(Error::new(
                bump_save.context(),
                "only one of account::bump andaccount:: bump_save may be present",
            ));
        }
        if self.bump_save.is_some() {
            return Err(Error::new(bump_save.context(), "duplicated attribute"));
        }

        self.bump_save.replace(bump_save.into_inner());
        Ok(())
    }

    pub fn add_belongs_to(&mut self, belongs_to: WithSpan<ConstraintBelongsTo>) -> Result<()> {
        let span = belongs_to.context();
        if self.belongs_to.insert(belongs_to.into_inner()) {
            Ok(())
        } else {
            Err(Error::new(span, "Duplicated account::belongs_to"))
        }
    }

    pub fn add_owner(&mut self, owner: WithSpan<ConstraintOwner>) -> Result<()> {
        if self.owner.is_some() {
            return Err(Error::new(owner.context(), "Duplicated account::owner"));
        }
        self.owner.replace(owner.into_inner());

        Ok(())
    }

    pub fn add_rent_exempt(&mut self, rent_exempt: WithSpan<ConstraintRentExempt>) -> Result<()> {
        if self.rent_exempt.is_some() {
            return Err(Error::new(
                rent_exempt.context(),
                "Duplicated account::rent_exempt",
            ));
        }
        self.rent_exempt.replace(rent_exempt.into_inner());

        Ok(())
    }

    pub fn add_expr(&mut self, expr: WithSpan<ConstraintExpr>) -> Result<()> {
        self.exprs.push(expr.into_inner());
        Ok(())
    }

    pub fn build(mut self) -> Result<PlainConstraints> {
        if self.init.is_some() && self.r#mut.is_none() {
            self.r#mut.replace(ConstraintMut);
        }

        if self.init.is_some() && self.owner.is_none() {
            self.owner.replace(ConstraintOwner::Program);
        }

        if self.init.is_some() && self.rent_exempt.is_none() {
            self.rent_exempt.replace(ConstraintRentExempt::Enforce);
        }

        Ok(PlainConstraints {
            init: self.init,
            is_mut: self.r#mut,
            signer: self.signer,
            address: self.address,
            seeds: self.seeds,
            bump: self.bump,
            bump_save: self.bump_save,
            belongs_to: self.belongs_to,
            owner: self.owner,
            rent_exempt: self.rent_exempt,
            exprs: self.exprs,
        })
    }
}

impl<'a> ToTokens for WithContext<&'a ConstraintInit, &'a PlainField> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        // Nothing to do?
    }
}

impl<'a> ToTokens for WithContext<&'a ConstraintMut, &'a PlainField> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        // Nothing because will be checked when it will try to actually write
    }
}

impl<'a> ToTokens for WithContext<&'a ConstraintSigner, &'a PlainField> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ident = &self.context.ident;
        let info = match self.context.ty {
            Ty::AccountInfo => quote! { #ident },
            Ty::ProgramAccount(_) => quote! { #ident.to_account_info() },
            Ty::ProgramState(_) => quote! { #ident.to_account_info() },
            _ => unreachable!(),
        };
        tokens.extend(quote! {
            // Don't enforce on CPI, since usually a program is signing and so
            // the `try_accounts` deserializatoin will fail *if* the one
            // tries to manually invoke it.
            //
            // This check will be performed on the other end of the invocation.
            if cfg!(not(feature = "cpi")) {
                if !#info.is_signer {
                    return Err(anchor_lang::solana_program::program_error::ProgramError::MissingRequiredSignature);
                }
            }
        })
    }
}

impl<'a> ToTokens for WithContext<&'a ConstraintAddress, &'a PlainField> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ident = &self.context.ident;
        let info = match self.context.ty {
            Ty::AccountInfo => quote! { #ident },
            Ty::ProgramAccount(_) => quote! { #ident.to_account_info() },
            Ty::ProgramState(_) => quote! { #ident.to_account_info() },
            _ => unreachable!(),
        };
        let check = {
            let decoded = bs58::decode(&self.value.value()).into_vec().unwrap(); // todo decode in parsing
            let span = self.value.span();
            quote_spanned! { span => anchor_lang::solana_program::pubkey::Pubkey::new(&[#(#decoded),*]) }
        };
        tokens.extend(quote! {
            if !#info.key != #check {
                return Err(anchor_lang::solana_program::program_error::ProgramError::Custom(1));
            }
        })
    }
}

impl<'a> ToTokens for WithContext<&'a ConstraintSeeds, &'a PlainField> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ident = &self.context.ident;
        let seeds = &self.seeds;
        if let Some(ConstraintBump { expr: bump }) = &self.context.constraints.bump {
            tokens.extend(
                quote! {
                    if *#ident.to_account_info().key != Pubkey::create_program_address(
                        &[#seeds, &[#bump]],
                        program_id,
                    ).map_err(|_| anchor_lang::solana_program::program_error::ProgramError::Custom(1))? {
                        return Err(anchor_lang::solana_program::program_error::ProgramError::Custom(1)); // todo
                    }
                });
        } else if let Some(ConstraintBumpSave { expr: bump }) = &self.context.constraints.bump_save
        {
            tokens.extend(quote! {
                {
                    let __key_bump = Pubkey::find_program_address(
                        &[#seeds],
                        program_id,
                    );
                    if *#ident.to_account_info().key != __key_bump.0 {
                        return Err(anchor_lang::solana_program::program_error::ProgramError::Custom(1)); // todo
                    }
                    #bump = __key_bump.1;
                }
            })
        } else {
            tokens.extend(quote! {
                if *#ident.to_account_info().key != Pubkey::find_program_address(
                    &[#seeds],
                    program_id,
                ).0 {
                    return Err(anchor_lang::solana_program::program_error::ProgramError::Custom(1)); // todo
                }
            })
        }
    }
}

impl<'a> ToTokens for WithContext<&'a ConstraintBelongsTo, &'a PlainField> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ident = &self.context.ident;
        let target = &self.inner.join_target;
        tokens.extend(quote! {
            if &#ident.#target != #target.to_account_info().key {
                return Err(anchor_lang::solana_program::program_error::ProgramError::Custom(1)); // todo: error codes
            }
        })
    }
}

impl<'a> ToTokens for WithContext<&'a ConstraintOwner, &'a PlainField> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ident = &self.context.ident;
        let info = match self.context.ty {
            Ty::AccountInfo => quote! { #ident },
            _ => quote! { #ident.to_account_info() },
        };
        let check = match self.inner {
            ConstraintOwner::Skip => return,
            ConstraintOwner::Program => quote! { *program_id },
            ConstraintOwner::Value(value) => {
                let decoded = bs58::decode(&value.value()).into_vec().unwrap(); // todo decode in parsing
                let span = value.span();
                quote_spanned! { span => anchor_lang::solana_program::pubkey::Pubkey::new(&[#(#decoded),*]) }
            }
        };
        tokens.extend(quote! {
            if *#info.owner != #check {
                return Err(anchor_lang::solana_program::program_error::ProgramError::Custom(1)); // todo: error codes
            }
        });
    }
}

impl<'a> ToTokens for WithContext<&'a ConstraintRentExempt, &'a PlainField> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ident = &self.context().ident;
        match self.inner {
            ConstraintRentExempt::Skip => {}
            ConstraintRentExempt::Enforce => tokens.extend(quote! {
                if !rent.is_exempt(#ident.to_account_info().lamports(), #ident.to_account_info().try_data_len()?) {
                    return Err(anchor_lang::solana_program::program_error::ProgramError::Custom(2)); // todo: error codes
                }
            }),
        }
    }
}
