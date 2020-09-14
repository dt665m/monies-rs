# monies-rs

Rust Money Crate 

## Concept

This crate revolves around the use of an `impl_money!` macro, which will automatically generate
a struct implementing useful money related operations such as splitting and allocating.  The idea
is to leverage code generation for type safe money operations.  

Usage Example:

```
use monies_macros::impl_money;

impl_money!(Bitcoin, "BTC", "9001", 8, "{} $");
impl_money!(USD, "USD", "840", 2, "$ {}");

fn main() {
  // Bitcoin { kind: "BTC", value: 0.00000001 }
  let one_satoshi = Bitcoin::from_minor(1);
  println!("{:?}", one_satoshi);

  // Bitcoin { kind: "BTC", value: 1 }
  let one_bitcoin = Bitcoin::from_major(1);
  println!("{:?}", one_bitcoin);

  // USDollar { kind: "USD", value: 1 }
  let one_quarter = USDollar::from_minor(25);
  println!("{:?}", one_quarter);
  
  // USDollar { kind: "USD", value: 1 }
  let one_dollar = USDollar::new(1);
  println!("{:?}", one_dollar);
}
```
