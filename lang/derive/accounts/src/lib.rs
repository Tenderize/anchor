extern crate proc_macro;

use anchor_syn::accounts::AccountsStruct;
use proc_macro::TokenStream;
use quote::ToTokens;
use syn::parse_macro_input;

/// Implements an [`Accounts`](./trait.Accounts.html) deserializer on the given
/// struct, applying any constraints specified via inert `#[account(..)]`
/// attributes upon deserialization.
///
/// # Example
///
/// ```ignore
/// #[derive(Accounts)]
/// pub struct Auth<'info> {
///     #[account(mut, belongs_to = authority)]
///     pub data: ProgramAccount<'info, MyData>,
///     #[account(signer)]
///     pub authority: AccountInfo<'info>,
/// }
///
/// #[account]
/// pub struct MyData {
///   authority: Pubkey,
///   protected_data: u64,
/// }
/// ```
///
/// Here, any instance of the `Auth` struct created via
/// [`try_accounts`](trait.Accounts.html#tymethod.try_accounts) is guaranteed
/// to have been both
///
/// * Signed by `authority`.
/// * Checked that `&data.authority == authority.key`.
///
/// The full list of available attributes is as follows.
///
/// | Attribute | Location | Description |
/// |:--|:--|:--|
/// | `#[account(init)]` | On `ProgramAccount` structs. | Marks the account as being initialized, skipping the account discriminator check. Implies mut and owner (if owner = skip is not present) |
/// | `#[account(mut)]` | On `AccountInfo`, `ProgramAccount` or `CpiAccount` structs. | Marks the account as mutable and persists the state transition. |
/// | `#[account(signer)]` | On raw `AccountInfo` structs. | Checks the given account signed the transaction. |
/// | `#[account(address = "<base58>")]` | On `AccountInfo`, `ProgramAccount` or `CpiAccount` structs. | Tests account key for equality |
/// | `#[account(seeds = [<seeds>, ...])]` | On `AccountInfo`, `ProgramAccount` or `CpiAccount` structs | Seeds for the program derived address an `AccountInfo` struct represents separated by comma. Every seed may be any rust expression of type `&[u8]`. May be used in conjunction with one of bump or bump_save |
/// | `#[account(bump = <bump_expr>)]` | On `AccountInfo`, `ProgramAccount` or `CpiAccount` structs | Expression of type `u8` used as bump seed for `Pubkey::create_program_address` when cheking seeds constraint |
/// | `#[account(bump_save = <bump_field>)]` | On `AccountInfo`, `ProgramAccount` or `CpiAccount` structs | Address of `u8` field inside mutable account to store result of `Pubkey::find_program_address` after checking it in seeds constraint |
/// | `#[account(belongs_to = <target>)]` | On `ProgramAccount` or `CpiAccount` structs | Checks the `target` field on the account matches the `target` field in the struct deriving `Accounts`. |
/// | `#[account(owner (= "<base58>" / program / skip)?)]` | On `AccountInfo`, `ProgramAccount` or `CpiAccount` structs. | Tests account's owner key for equality |
/// | `#[account(rent_exempt( = <skip>)?)]` | On `AccountInfo` or `ProgramAccount` structs | Optional attribute to skip the rent exemption check. By default, all accounts marked with `#[account(init)]` will be rent exempt, and so this should rarely (if ever) be used. Similarly, omitting `= skip` will mark the account rent exempt. |
/// | `#[account(expr = <expr>)]` | On any type deriving `Accounts` | Executes the given expression. Return `Err(ProgramError::..)` if check is failed |
/// | `#[account(executable)]` | On `AccountInfo` structs | Checks the given account is an executable program. |
#[proc_macro_derive(Accounts, attributes(account))]
pub fn derive_anchor_deserialize(item: TokenStream) -> TokenStream {
    let strct = parse_macro_input!(item as AccountsStruct);
    strct.to_token_stream().into()
}
