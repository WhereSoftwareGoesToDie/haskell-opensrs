# OpenSRS API for Haskell

This is a Haskell library for interacting with the [OpenSRS][] wholesale domain
name registration API.

© 2014 [Anchor Systems][] Pty Ltd and Others

[![Build Status](https://travis-ci.org/anchor/haskell-opensrs.svg)](https://travis-ci.org/anchor/haskell-opensrs)

## Usage

```haskell
let config = SRSConfig "https://horizon.opensrs.net:55443" "myusername"
        "mykey" "127.0.0.1"

res <- doRequest $ LookupDomain config "bigfatchunkybear.info"

res2 <- doRequest $ GetDomain config "hobbseetest.com"

res3 <- doRequest $ ModifyDomain config "im-a-little-teapot.com" False
        (fromList [("data", "whois_privacy_state"), ("state", "enable")])
        Nothing
```

## Installation for usage

* `cabal install opensrs`

## Installation for development

* Copy `tests/TestConfig.hs.example` to `tests/TestConfig.hs`
* Customise `tests/TestConfig.hs` to point to your development instance
* `cabal install --enable-tests`

[OpenSRS]: https://opensrs.com/
[Anchor Systems]: http://www.anchor.com.au/
