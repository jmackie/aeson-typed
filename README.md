# Warning: probably a terrible idea

This was a little experiment in capturing JSON serialization logic at the type-level. 

_Why tf would you want to do that?_ I hear you ask.

Well suppose you were writing a nice REST API. If you were going ham you probably want some nice swagger/OpenAPI docs, JSON schemas, generated typescript types and/or client code, etc etc.
All of these things depend on the details of your [`Aeson.ToJSON`](https://hackage.haskell.org/package/aeson-1.5.4.1/docs/Data-Aeson.html#t:ToJSON) and [`Aeson.FromJSON`](https://hackage.haskell.org/package/aeson-1.5.4.1/docs/Data-Aeson.html#t:FromJSON) instances.
Sure, you could keep these things in sync manually (and tbh, in the interest of simplicity, you probably should). But what if we could have GHC do it for us...

[See here for what this might look like.](https://github.com/jmackie/aeson-typed/blob/master/test/Data/SimpleObject.hs)

**Disclaimer: This is a work in progress but there's very little work and it's not really in progress**
