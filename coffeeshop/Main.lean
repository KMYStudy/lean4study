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

inductive Lista
  | nil
  | cons (head: Nat) (tail: Lista)

def ListaSize : Lista -> Nat
  | Lista.nil => 0
  | Lista.cons _ tail => 1 + ListaSize tail

def bigList : Lista := Lista.cons 1 (Lista.cons 2 (Lista.cons 3 Lista.nil))

def emptyList : Lista := Lista.nil

def threeElementsList : Lista := Lista.cons 1 (Lista.cons 2 (Lista.cons 3 Lista.nil))

def listLength : Lista -> Nat
  | Lista.nil => 0
  | Lista.cons _ tail => 1 + listLength tail

#eval listLength Lista.nil
#eval listLength threeElementsList

def printList : Lista -> String
  | Lista.nil => ""
  | Lista.cons head tail => toString head ++ " " ++ printList tail

#eval printList threeElementsList

def concatList : Lista -> Lista -> Lista
  | Lista.nil, y => y
  | Lista.cons x xs, y => Lista.cons x (concatList xs y)

class Priceable (α: Type) where
  price : α -> Nat

instance : Priceable Coffee where
  price
    | Coffee.espresso => 2
    | Coffee.cappuccino => 3
    | Coffee.latte => 3
    | Coffee.mocha => 4

instance : Priceable Donut where
  price donut :=
    let flavorPrice :=
      match donut.flavor with
        | Flavor.vanilla => 1
        | Flavor.chocolate => 2
        | Flavor.strawberry => 2
    let sprinklePrice := if donut.sprinkles then 1 else 0
    flavorPrice + sprinklePrice
