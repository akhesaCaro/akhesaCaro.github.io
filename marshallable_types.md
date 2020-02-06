# Marshallable types/objects

## What is a Marshallable type/object?

Some will call it DAO/DTO, others published language or anything else. We will call `marshallable objec` a type or an object used to put a wall between your domain modelisation and anything that could disrupt it (JSON, Database, an other domain, etc...). 

## Why using marshallable type?



### Avoid corrumpting you model

It easy to be tempted by using the exact same schema as your database or your transferable object (for examle JSON) for your domain type.

#### Price problem

Price are not easy to handle. You have this precision problem, and sometimes your need to carry the currency of your money.

Let say you need to handle the amount and the currency. Your database will probably looks like this : 

```SQL
CREATE TABLE myProduct (
	id SERIAL PRIMARY KEY,
    name text,
	price_in_cent integer NOT NULL,
	currency text NOT NULL,	
);
```


But I have this very useful instance to serialize directly from my type/object to my database.


```Haskell
data MyProductDB = MyProductDB { id :: UUID
                               , name :: String
                               , priceInCents :: Integer
                               , currency :: String
                               }
                               deriving (ToRow, FromRow)
```

In the other hand, I have a very specific type/object which can handle more accuratly my price in my domain side.


```Haskell
-- safe-money's Dense type

> 2 :: Dense "EUR"
Dense "EUR" 2%1
```

or a more simpler type/object, still accurate

```Haskell
data Price = Price { amount :: Rational
currency :: String }
```

Okay, I can construct an other type/object with my specific type and have conversions functions allowing me to map both.

```Haskell

data MyProduct = MyProduct { id :: UUID
                           , name :: String
                           , price :: Price
                           }

fromMyProductDB :: MyProductDB -> MyProduct
toMyProductDB :: MyProduct -> MyProductDB
```

### Have the liberty to change the representation afterward

The main advantage to use marshallable type is to have the liberty to change the presentation of your database or your json, your model, etc




Sometimes you don't need .... yet ... different type/object but want to have the liberty afterward when it will evoluate.

*Haskell's tips*

You can encapsulate your marshallable's type into your model type with a `newtype` (and not the other way around).

It is more likely that your domain will change. So this way, you will know if you need to break the link between them.

```Haskell

data PersonDB = PersonDB { id :: UUID
                         , name :: String
                         , email :: String
                         }
                         deriving (ToRow, FromRow)
                     
                     
newtype Person = Person { getPerson :: PersonDB }

```
