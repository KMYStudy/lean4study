inductive Coffee
  | espresso
  | latte
  | cappuccino
  | mocha

def myFavoriteCoffee : Coffee :=
  Coffee.cappuccino

def coffeePrice (coffee: Coffee) : Nat :=
  match coffee with
  | Coffee.cappuccino => 2
  | Coffee.latte => 3
  | Coffee.mocha => 3
  | Coffee.espresso => 4

def coffeePrice2 : Coffee -> Nat
  | Coffee.cappuccino => 2
  | Coffee.latte => 3
  | Coffee.mocha => 3
  | Coffee.espresso => 4

def Coffee.price : Coffee -> Nat
  | cappuccino => 2
  | latte => 3
  | mocha => 3
  | espresso => 4

#eval coffeePrice Coffee.espresso
#eval coffeePrice2 Coffee.latte
#eval Coffee.price Coffee.mocha

inductive Flavor
  | chocolate
  | vanilla
  | strawberry

structure Donut where
  flavor : Flavor
  sprinkles: Bool

def strawberryDonutWithFlavor : Donut :=
  { flavor := Flavor.strawberry, sprinkles := true }

def someFunction : Nat -> Nat -> Nat := λx => λy => x + y

def myFavoriteDonut : Donut :=
  { flavor := Flavor.chocolate, sprinkles := true }

inductive Product
  | coffee (c: Coffee)
  | donut (d: Donut)

def meal : Product :=
  Product.coffee Coffee.espresso

def donut : Product :=
  Product.donut myFavoriteDonut

def main : IO Unit :=
  IO.println "Hello world"
