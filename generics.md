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
data Product = 
    Product { id :: UUID
            , label :: String
            , price :: Dense
            , description :: String
            }

```

Now we need to store this record into a Database. For istance, let say that we have a `PostgreSQL` database and we are using [postgresql-simple](https://hackage.haskell.org/package/postgresql-simple-0.6.2/docs/Database-PostgreSQL-Simple.html). We will need a `FromRow` and `ToRow` instance for our type.

But the `Dense` type contains 2 **essentials** information that we need to store : The _amount_ **and** the _currency_.
We will than need 2 columns in our database : 

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



```


But, when we look at it, the lecture of the produced SQL schema is not that obious...

What if we create a separate type for the Database and use an Generic instance instead and some mapping functions to transform our "marshallable type" from and to our domaine type.

```haskell
IMPORT

data ProductJSON = ProductJSON 
  { id :: UUID
  , label :: String
  , amount :: Int
  , currency :: String
  , description :: String
  } deriving Generic, FromRow, ToRow

productToProductJSON :: Product -> ProductJSON

productJSONToProduct :: ProductJSON -> Product

```

Great! Now we have an _exact reprsentation_ of the produced database schema. We don't have to overload our mind to "run the algortihm" and have a mind representation of the produced schema.

## Generics as an architecture design

As we saw earlier, using `Generics` naturally made us separate the domain types from the interface ones. In our case `Product` and `ProductJSON`.

Interfaces are somehow contracts with the outside world. Sometimes we have the control it, sometimes we don't. It will be easier to control a database than an API client.

We could say that interfaces have **a stricter constraint**, because if something is changed, as little as it could be, it could broke something elsewhere... The worst of it, our beloved compiler will not even be able to notice the broken issue. :fear:

I think its a good thing to separate the domain world for the interfaces with different types. This way, it's more reassuring to change something and it is obiously easier to restraint the change scope than with a mega-type design.

If you need to change something in the sensible part (interface), you will be consciously more careful to not broke _THE_ contract.

** GRAPH? **

This way of designing code reminds me what Domaine Driven Development enthousiasts call **Hexagonal Architecture** (alystar article).

So `Generics` tend to help you to architecture your code while naturraly separing the interfaces from the core (domain) and make your code more layered!

//Interation compilation???

## Generics as a lingua franca

Okay, so we have the documentation, we have a neat design, what else could Generics give us?

The way Generics work is that it create a ____ of the type. This ___ is useful for anyone who need to transform. 

Instead to have a mapping between the origin and the destination (for example a List to a JSON), you have a representation of it. So if you need to code a converter, it won't broke on the simple change. 

### Code generator

It exists a lot of code generator, specially for API client accessors.

For example, from an Haskell `Servant` API, you could generate a Purescript (LINK) or a ELM (LINK) client, and keep the type safety that we all love. But the requirement for thoses nice libraries to work is that your type need to implements `Generics`.

### Swagger generator

If you did work with APIs, you probably heard or used swagger(link) as an API documentation. You can obiously generate this documentation, if you implement generics with this library (LINK).

BUT! Another nice thing about Swagger is that its shared in the whole community, technologicallywise. So if you setup a public API with Swagger, your client could generate their accessors (LINK), even in non FP technologies.

In the API world, `Generics` give you a lingua franca for your client! 

## Conclusion

In summary, in addition of being a **boilerplate saver**, `Generics` give you a lot a other nice features, such as : 

- Direct interface documentation in your code (any kind of interface).
- Some robus client accessor code generators (API). LINK
- A Swagger documentation generator (API). LINK
- And once you have a Swagger documentation, you have a lingua franca for any other technology that needs to access your API.

I am sure there are some cases where `Generics` are not the best options for an interface implement, but most of the time, theses advantages worth to think about it before implementing a custom convertor.
