# Generics


When I say `Generics`, you probably think "boilerplate saver". Its true, `Generics` could avoid you a lot of work when you need to transform a type to communicate outside your application. But, what if `Generics` give us more than that?

`Generics` can be seen as a good way to document your interface, to have a code generator for your client (if an API is implemented) or just a way to architect the code and separate the domain from the interfaces.

However, this article is not about how the `Generics` work, but about the many and very useful "side effects" that you gain using them.

## Generics for Documentation

To be efficient, the type implementing the `Generic` instance must be as close as possible to the wanted representation.

For example, imagine we have a _product_ with a _price_ in our domain and we use the [safe-money library](htytps://ren.zone/articles/safe-money) to represent it because we care about the _currency_. 

```Haskell
> 2 :: Dense "EUR"
Dense "EUR" 2%1    -- EUR 2.00
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

Now we need to store this record into a Database. For istance, let say that we have a `PostgreSQL` database and we are using [postgresql-simple](https://hackage.haskell.org/package/postgresql-simple-0.6.2/docs/Database-PostgreSQL-Simple.html). We will need a `FromRow` and `ToRow` instance for our type.

But the `Dense` type contains 2 **essentials** information that we need to store : The _amount_ **and** the _currency_.
We are going to need 2 columns in our database : 

```Sql
CREATE TABLE product (
    id SERIAL PRIMARY KEY,
    label text NOT NULL,
    price_in_cent integer NOT NULL,
    currency text NOT NULL
);
```

We could write a custom implementation for our `Product` type.


```Haskell
{-# LANGUAGE DataKinds #-}

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


But, when we look at it, the lecture of the produced SQL schema is not that obious...

And we have to be careful to make lawfull instancies (`toRow . fromRow = id`, `fromRow . toRow = id`)

What if we create a separate type for the Database and use an Generic instance instead? We will obiously need some mapping fonctions, but the `RecordWildCards` extension can make this easier. Let see :

```haskell
data ProductDB = ProductDB -- This is our documentation
  { id :: Int
  , label :: String
  , amount :: Int
  , currency :: String
  , description :: String
  } deriving Generic, FromRow, ToRow

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

Great! Now we have an _exact reprsentation_ of the produced database schema. We don't have to overload our mind to "run the algortihm" and have a mind representation of the produced schema.

What if my domain type has the same exact representation than my Database schema or my JSON, I don't need a marshallable type and, don't I?

Well you don't need it ... yet, but maybe you will one day. Domain tend to change over time and separate the domain from the interface could be useful. We will se in the next section. 

## Generics as an architecture design

As we saw earlier, using `Generics` naturally made us separate the domain from the interface. In our case `Product` and `ProductDB`.

Interfaces are somehow contracts with the outside world. Sometimes we have the control on them, sometimes we don't. It will be easier to control a database than an API client, for instance.

We could say that interfaces have **a stricter constraint**, because if something is changed, as little as it could be, it could broke something elsewhere... The worst of it... our beloved compiler will not even be able to notice the broken issue. :fear:

I think its a good thing to separate the domain world from the interfaces with different types. This way, it's more reassuring to change something and it is obiously easier to restraint the change scope than with a mega-type design.

If you need to change something in the sensible part (interface), you will be consciously more careful to not broke _THE_ contract.

** GRAPH? **

This way of designing code reminds me what Domaine Driven Development enthousiasts call [Hexagonal Architecture](https://alistair.cockburn.us/hexagonal-architecture/).

So `Generics` tend to help you to architecture your code while naturraly separing the interfaces from the core (domain) and make your code more layered!

Is someday you need the change all the layers, you can easily do _incremental compilation_ by change a layer at a time.

## Generics as a lingua franca

Okay, so we have the documentation, we have a neat design, what else could **Generics** give us?

The way Generics work is that it creates a **structure representation** of the type. Instead of having a mapping between the origin and the destination (for example a List to a JSON), you have a representation of it. So if you need to code a converter, it won't broke on the simple change. 

### Code generator

Many code generators working with `Generics` are availables. Particulary for API client accessors.

For example, from an Haskell `Servant` API, you could generate a [Purescript accessors functions](https://hackage.haskell.org/package/servant-purescript) or a [ELM accessors functions](https://hackage.haskell.org/package/servant-elm) client, and keep the type safety that we all love. But the requirement for thoses nice libraries to work is that your type need to implements `Generics`.

### Swagger generator

If you did work with APIs, you probably heard or used [swagger](https://swagger.io/) as an API documentation. You can obiously generate this documentation, if you implement generics with [servant-swagger](https://github.com/haskell-servant/servant-swagger).

BUT! Another nice thing about Swagger is that its shared by the whole community, technologicallywise. So if you setup a public API with Swagger, anyone who wants to use it generate their [accessors](https://github.com/swagger-api/swagger-codegen), even in non FP technologies.

In the API world, `Generics` give you a lingua franca for your client! 

## Conclusion

In summary, in addition of being a **boilerplate saver**, `Generics` give you a lot a other nice features, such as : 

- Direct interface documentation in your code (any kind of interface).
- Some robus client accessor code generators (API). 
- A Swagger documentation generator ([servant-swagger](https://github.com/haskell-servant/servant-swagger))
- And once you have a Swagger documentation, you have a [lingua franca](https://github.com/swagger-api/swagger-codegen) for any other technology that needs to access your API.

I am sure there are some cases where `Generics` are not the best options for an interface implemention, but most of the time, theses advantages worth to think about it before implementing a custom convertor.
