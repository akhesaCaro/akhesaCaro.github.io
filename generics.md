# Generics

Generics in Haskell are a mechanism through which datatype can “expose” their internal structure, allowing programmers to write functions operating on (almost) arbitrarily-shaped data. Tons [lien] and tons [lien] of articles have been written on how generics work. 

In this article, we will instead focus on how they can be used, and the benefits for your code.»
You probably think of Generics as boilerplate savers, but it is more than that. Generics can help you gain a self maintained documentation, robustess on your code and a nice layered archtecture.

## Generics for ...

As Haskellers, one of our main goal is to model the domain as close as possible with types. But unfortunatly, the well designed domain must interact with the outside world... and this world, we sometimes don't have any control on it.

For example, imagine we have a legacy PostgreSQL database to interact with. The database store products, these products have a price, and this price need to deal with different currencies. The table looks like this : 

```Sql
CREATE TABLE product (
    id SERIAL PRIMARY KEY,
    label text NOT NULL,
    price_in_cent integer NOT NULL,
    currency text NOT NULL
);
```

We chose to model this `Product` with the [safe-money library](htytps://ren.zone/articles/safe-money) library, for instance.


```Haskell
-- The Discrete is represented by its currency, its scale and amount.
> 1 :: Discrete "EUR" "cent"
Discrete "EUR" 100%1 1           -- 0.01 EUR
```

Our domain type could looks like this : 

```Haskell
{-# LANGUAGE DataKinds #-}

import Money

data Product = 
    Product { id :: Int
            , label :: String
            , price :: Dense "EUR"
            , description :: String
            }

```
To interact with the database, let say we are using [postgresql-simple](https://hackage.haskell.org/package/postgresql-simple-0.6.2/docs/Database-PostgreSQL-Simple.html).

The `Discrete` will need to be splitted to be stored as 2 differents columns, _amount_ and the _currency_, as our database requires.

We could write custom `FromRow` and `ToRow` instances for our `Product` to be able to split it.

```Haskell
{-# LANGUAGE DataKinds #-}

import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.toRow

instance ToRow Product where
 toRow product =
   [ toField (productId product)
   , toField (label product)
   , toField (toInteger $ price product)
   , toField (discreteCurrency $ price product)
   , toField (description product)
   ]

instance FromRow Product where
  fromRow = do
   let unitScale = scale (Proxy :: Proxy (UnitScale "EUR" "cent"))  --Used scale
   sd <- mkSomeDiscrete <$> field <*> pure unitScale <*> field      --Function for serialization
   case fromSomeDiscrete sd of                                      --Transformation to Maybe Discrete
     Nothing -> fail "Currency not supported"
     Just val -> Product <$> field <*> field <*> pure val <*> field --Product creation
```

But we have 2 problems here : 

1. First of all, when we look at the custom instanciations, the produced SQL schema is not that obious... and as programmer I will probably have to check the database script each time I need t have a mind representation of the interface.
2. Second, and probably worst, I have no garantie that my two instanciations are in phase. Remember the two functions must follow identity laws : (`fromRow . toRow <=> id`) and (`toRow . fromRow <=> id`). If you are not convinced that such error could occur, I did myself make that mistake by writing this article. Imagine on a very complex code... 


## ... documentation

To solve the first problem, I could probably create a **separate type** for the serialization to have a more self contained documentation. The splitting could be helded by the mapping functions.


```haskell
{-# LANGUAGE DataKinds #-}

data ProductDB = ProductDB 
  { id :: Int
  , label :: Text
  , amount :: Integer
  , currency :: Text
  , description :: Text
  }

toProductDB :: Product -> ProductDB
toProductDB Product{..} =
  let amount = toInteger price
      currency = discreteCurrency price
  in ProductDB{..}
  

data Errors = NOT_SUPPORTED_CURRENCY

fromProductDB :: ProductDB -> Either Errors Product -- I have a better "fail faster" way that I will talk in another article.
fromProductDB ProductDB {..} =
  case fromSomeDiscrete sd of
    Nothing -> Left NOT_SUPPORTED_CURRENCY
    Just (price :: Discrete "EUR" "cent") -> Right Product{..}
  where unitScale = scale (Proxy :: Proxy (UnitScale "EUR" "cent"))  --Used scale
        sd = mkSomeDiscrete currency unitScale amount

instance ToRow ProductDB where
 toRow product =
   [ toField (productId product)
   , toField (label product)
   , toField (toInteger $ price product)
   , toField (discreteCurrency $ price product)
   , toField (description product)
   ]

instance FromRow ProductDB where
  fromRow = ProductDB <$> field <*> field <*> field <*> field <*> field
```

Great, we have our self-documented type now, it solves the first problem, does it?. 

Not really.. a time constraint or lazy programmer may have edited the instanciations without changing the type.

And we still have the second problem, this type doesn't guarantees robustness of our instanciations.

## ... documentation AND robustness

If we use _`Generic`_ and a _separate type_ we solve both problems with the same shot. 

```haskell
data ProductDB = ProductDB -- This is our documentation
  { id :: Int
  , label :: Text
  , amount :: Integer
  , currency :: Text
  , description :: Text
  } deriving Generic, (FromRow, ToRow)

toProductDB :: Product -> ProductDB
toProductDB Product{..} =
  let amount = toInteger price
      currency = discreteCurrency price
  in ProductDB{..}

data Errors = NOT_SUPPORTED_CURRENCY

fromProductDB :: ProductDB -> Either Errors Product -- I have a better "fail faster" way that I will talk in another article.
fromProductDB ProductDB {..} =
  case fromSomeDiscrete sd of
    Nothing -> Left NOT_SUPPORTED_CURRENCY
    Just (price :: Discrete "EUR" "cent") -> Right Product{..}
  where unitScale = scale (Proxy :: Proxy (UnitScale "EUR" "cent"))  --Used scale
        sd = mkSomeDiscrete currency unitScale amount
```

Great! Now we have an _exact reprsentation_ of the produced database schema. We don't have to overload our mind to "run the algortihm" to have a representation of the produced schema.


But... What if my domain type has the same exact representation than my Database schema or my JSON, I don't need a marshallable type, don't I?

Well you don't need it ... yet, but maybe you will one day. Domain tend to change over time and separate the domain from the interface as soon as possible could avoid you a lot of problems. 

## Generics as an architecture design

As we saw earlier, using `Generics` naturally made us separate the domain from the interface. In our case `Product` and `ProductDB`.

Interfaces are somehow contracts with the outside world. Sometimes we have the control on them, sometimes we don't. It will be easier to control a database than an API client, for instance.

We could say that interfaces have **a stricter constraint**, because if something is changed, as little as it could be, it could broke something elsewhere... The worst of it... our beloved compiler will not even be able to notice the broken issue. :fear:

I think its a good thing to separate the domain world from the interfaces with different types. This way, it's more reassuring to change something and it is obiously easier to restraint the change scope than with a "mega-type" design.

If you need to change something in the sensible part (interface), you will be consciously more careful to not break _THE_ contract.

** GRAPH? **

This way of designing code reminds me what Domaine Driven Development enthousiasts call [Hexagonal Architecture](https://alistair.cockburn.us/hexagonal-architecture/).

So `Generics` tend to help you to architecture your code by naturraly separing the interfaces from the core (domain) and make your code more layered!

If someday you need the change all the layers, you can easily do _incremental compilation_ by changing a layer at a time.

## Generics as a lingua franca

Okay, so we have the documentation, we have a neat design, what else could **Generics** give us?

The way Generics work is that it creates a **structure representation** of the type. Instead of parsin the type and give the correspondance, it gives a "description" of the type.

### Code generator

Many code generators working with `Generics` are availables. Particulary for API client accessors.

For example, from an Haskell `Servant` API, you could generate [Purescript accessors functions](https://hackage.haskell.org/package/servant-purescript) from a `servant` API or a [ELM accessors functions](https://hackage.haskell.org/package/servant-elm). This way, it is easier to kepp the typesafety we all love. To use them, the type must implements `Generics`.

### Swagger generator

If you did work with APIs, you probably heard or used [swagger](https://swagger.io/) as an API documentation. You can obiously generate this documentation, if you implement generics with [servant-swagger](https://github.com/haskell-servant/servant-swagger).

BUT! Another nice feature of Swagger, is its whole community shared way to document an API, technologicallywise. So if you setup a public API with Swagger, anyone who wants to use it could generate their [accessors](https://github.com/swagger-api/swagger-codegen) with the language they want, even in non FP technologies.

In the API world, `Generics` give you a "lingua franca" for your client! 

## Conclusion

In summary, in addition of being a **boilerplate saver**, `Generics` give you a lot a other nice features, such as : 

- Direct interface documentation in your code (any kind of interface).
- Some robus client accessor functions code generators (API). For instance for [Purescript](https://hackage.haskell.org/package/servant-purescript) or [ELM](https://hackage.haskell.org/package/servant-elm). 
- A Swagger documentation generator ([servant-swagger](https://github.com/haskell-servant/servant-swagger))
- And once you have a Swagger documentation, you have a [lingua franca](https://github.com/swagger-api/swagger-codegen) for any other technology that needs to access your API.

I am sure there are some cases where `Generics` are not the best options for an interface implemention, but most of the time, theses advantages worth to think about it before implementing a custom convertor.
