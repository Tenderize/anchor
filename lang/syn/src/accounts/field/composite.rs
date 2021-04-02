use std::convert::TryFrom;

use super::{super::constraints::ConstraintExpr, AccountFieldGenerator, MetaAccountFieldGenerator};
use crate::{
    accounts::{constraints::Constraint, is_account_attr},
    WithContext, WithSpan,
};
use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{
    parse2, punctuated::Punctuated, spanned::Spanned, token::Comma, Attribute, Error, Field, Ident,
    Result,
};
#[derive(Debug)]
pub struct CompositeField {
    pub ident: Ident,
    pub symbol: String,
    pub constraints: CompositeConstraints,
    pub raw_field: Field,
}

impl MetaAccountFieldGenerator for CompositeField {
    fn to_account_metas(&self) -> TokenStream {
        let name = &self.ident;
        quote! {
            account_metas.extend(self.#name.to_account_metas(None));
        }
    }

    fn field(&self) -> TokenStream {
        let name = &self.ident;
        let symbol = Ident::new(&format!("{}Meta", self.symbol), Span::call_site());

        quote! {
            pub #name: #symbol,
        }
    }
}

impl AccountFieldGenerator for CompositeField {
    fn deser(&self) -> TokenStream {
        let name = &self.ident;
        let ty = &self.raw_field.ty;
        quote! {
            let #name: #ty = anchor_lang::Accounts::try_accounts(program_id, accounts)?;
        }
    }

    fn access_checks(&self) -> TokenStream {
        WithContext::new(&self.constraints, self).to_token_stream()
    }

    fn to_account_infos(&self) -> TokenStream {
        let ident = &self.ident;
        quote! {
            account_infos.extend(self.#ident.to_account_infos());
        }
    }

    fn to_account_metas(&self) -> TokenStream {
        let name = &self.ident;
        quote! {
            account_metas.extend(self.#name.to_account_metas(None));
        }
    }

    fn on_save(&self) -> TokenStream {
        let ident = &self.ident;
        quote! {
            anchor_lang::AccountsExit::exit(&self.#ident, program_id)?;
        }
    }
}

fn ident_string(f: &syn::Field) -> String {
    let path = match &f.ty {
        syn::Type::Path(ty_path) => ty_path.path.clone(),
        _ => panic!("invalid account syntax"),
    };
    // TODO: allow segmented paths.
    assert!(path.segments.len() == 1);
    let segments = &path.segments[0];
    segments.ident.to_string()
}

impl TryFrom<Field> for CompositeField {
    type Error = Error;

    fn try_from(field: Field) -> Result<Self> {
        let mut constraints_builder = CompositeConstraintsBuilder::default();
        for attr in field.attrs.iter().filter(is_account_attr) {
            constraints_builder.add_attr(attr)?;
        }
        Ok(Self {
            ident: field.ident.as_ref().unwrap().clone(),
            symbol: ident_string(&field),
            constraints: constraints_builder.build()?,
            raw_field: field,
        })
    }
}

#[derive(Debug, Default)]
pub struct CompositeConstraints {
    exprs: Vec<ConstraintExpr>,
}

impl<'a> ToTokens for WithContext<&'a CompositeConstraints, &'a CompositeField> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for expr in &self.inner.exprs {
            expr.to_tokens(tokens);
        }
    }
}

#[derive(Debug, Default)]
pub struct CompositeConstraintsBuilder {
    exprs: Vec<ConstraintExpr>,
}

impl CompositeConstraintsBuilder {
    pub fn add_attr(&mut self, attr: &Attribute) -> Result<()> {
        let components = attr.parse_args_with(Punctuated::<Constraint, Comma>::parse_terminated)?;

        for component in components {
            self.add_constraint(component)?;
        }

        Ok(())
    }

    pub fn add_constraint(&mut self, constraint: Constraint) -> Result<()> {
        match constraint {
            Constraint::Expr(expr) => self.add_expr(*expr),
            _ => Err(Error::new(
                constraint.span(),
                "Invalid constraint for composite field",
            )),
        }
    }

    pub fn add_expr(&mut self, expr: WithSpan<ConstraintExpr>) -> Result<()> {
        self.exprs.push(expr.into_inner());
        Ok(())
    }

    pub fn build(self) -> Result<CompositeConstraints> {
        Ok(CompositeConstraints { exprs: self.exprs })
    }
}
