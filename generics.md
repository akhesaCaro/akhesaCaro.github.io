# Generics is not just about boilerplate saving

Generics are very useful to avoid a lot of boilerplate.

https://blog.clement.delafargue.name/posts/2019-09-10-a-new-tale-of-servant-clients.html

You can see them as a way to document your interface and separate them from your core code (domain).

There is two ways to connect your code with, for instance, your Servant (API).
You can simply use your domain type, and implement ToJSON and FromJSON typeclass.
Or you can contruct a type that represent your physical representation of your JSON and derive Generic, JSON and FromJSON directly.

Let see the difference : 

Imagine a simple domain type : 


```Haskell

data Product = Product { id :: UUID
                       , label :: String
                       }
                       
data Price  Price { amount :: Rational
                  , currency :: Currency 
                  
data Currency = EUR | USD

``` 

for instance, imagine you need to arrange you type and add an URL to the JSON for your API :

We can write the ToJSON instance this way : 

```Haskell
instance ToJSON Product where
  toJSON (Product id label (Price amount currency)) = 
    object [ "id" .= id
           , "label" .= label
           , "amount" .= amount
           , "currency" .= show currency
           , "categoryURL" .= makeCategoryURL id
           ]
```


```Haskell

data ProductJSON = ProductJSON { id :: String
                               , label :: String
                               , amount :: Float
                               , currency :: String
                               , categoryURL :: String
                               }
```

You see directly what your JSON will looks like. Your generic type act as a Interface Documentation.

Naturally, the generic type will need to be mapped to a type that represent the domain accuratly. And it is another cool thing. 

Your interface and core layer will be will separated.

Your interface is subject to more stringent constraints since modifying it may prevent third-party applications









But, you will tell me, why if I have a 1:1 mapping between my JSON and my Domain type.

Well, why not seperate the two layers?

It is totally free with the RecordWildCards and protect your the 2 layers from corrupting each other.

This way, you can't accidently change the JSON by changing your domain representation and vice and versa.

``` Haskell
{-# LANGUAGE RecordWildCards #-}

fromProduct

toProduct
```

Sometimes you will need to check the data before expose it to your system. 

Imagine you only accept "EUR" currency. You will have to check it and fail _before_ transforming your type into your domain type (fail fast!). 

I will explain a technic to do this in another article.



--- 

Les generics sont souvent utilisé pour gagner du temps pour transformer les types afin de les envoyer quelque part?  Par exemple, créer un JSON automatiquement à partir d'un type, ou une requête SQL.

Mais est-ce que c'est le seul avantage? Gagner du temps en évitant de s'embêter avec le boiler plate?

Cet article s'attardera sur ce que les Generic apporte de plus que d'éviter le boiler plate à savoir : 

La difficulté avec les interfaces externes, c'est que le language parlé est souvent divergent du modèle métier.


  - Le gain instantané d'une documentation d'interface.
- La génération automatique de documentation.
https://github.com/swagger-api/swagger-codegen
https://github.com/haskell-servant/servant-swagger
- La génération automatique de client.
https://hackage.haskell.org/package/servant-purescript
- La séparation naturelle entre les interfaces et la couche métier du code.



It is more than a boilerplate saver. Gerenics can be used as a document as well.

It could be hard to transform directly your domain model into your your transferrable object domain (JSON) or your schema (SQL) and could be tempting to write your own instance instead of using Generic.

## Generics for documentation

But if you separate your domain model type from the type implementing the Generic, your will gain some useful "side effects"

First of all, you will have a type that represent _exactly_ your transferrable object or your schema. The type itself act as the *documentation* of the interface.

It could be mental overloading to represent you the result of your serialization if you wrote a custom --transformation instanciation--. With a type dedicated for the serialization, you have the exact representation of it _directly_.


## Separation of your domain model and the rest of the world.

If you use a dedicated type for your serialization, that means that you domain representation is completly uncouple with the rest of the world. You can, that way, be reasure that you will not brake your contract interface, if you need to rename of change something.

## Generics as a lingua franca for APIs

As we said earlier, your dedicated type could act as a documentation, but you could also generate that documentation for an API for instance :

https://github.com/haskell-servant/servant-swagger


This swagger documentation can be see as a lingua franca allowing you to talk with the rest of the world. A lot of



Better than this, you could generate code for other world such as an automatic client accessor.

For Purescript https://hackage.haskell.org/package/servant-purescript

For ELM : https://hackage.haskell.org/package/servant-elm





instanciation custom : 

Les types ne sont pas obious, si elle est compliqué, tu dois faire l'algorithme dans ta tête


La conversion n'est pas seulement un mapping entre nos types et la représentation, mais aussi entre nos types et nos métiers.


We try to connect two worlds via JSON. Sometimes they are strickly identical, sometimes the models really differs.

JSON serializer doesn't know about your model.

"The hints you gave it may become outdated"

"to change your external contract when your internal representation changes? is that what you want?"






When I say `Generics`, you probably think "boilerplate saver". Its true, `Generics` are time saverq when you need to transform a type to communicate outside your application. But, what if `Generics` give us more than this?

`Generics` can be seen as a good way to document your interface or to have a code generator for your client (if an API is implemented) or just a way to architect your code and separate your domain from the interface.

However, this article is not about how the `Generics` work, but about the many and very useful "side effects" that you gain using them.

## Generics for Documentation

To be efficient, the type implementing the `Generic` instance must be as close as possible to the wanted representation.

For example, imagine we need to manipulate a type with a _price_ in our domain and we use the [safe-money library](htytps://ren.zone/articles/safe-money). 

```Haskell
> 2 :: Dense "EUR"
Dense "EUR" 2%1    -- EUR 2.00
```

You domain type could looks like this : 

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
CREATE TABLE product(
    ProducID int,
    Label varchar(255),
    Amount int,
    Currency String
)
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
