# OpenSRS API for Haskell

© Anchor Systems 2014

## Usage

```haskell
let config = SRSConfig "https://horizon.opensrs.net:55443" "myusername" "mykey" "127.0.0.1"

let req = LookupDomain config "bigfatchunkybear.info"
res <- doRequest req

let req2 = GetDomain config "hobbseetest.com"
res2 <- doRequest req2
```

## Installation for usage

* `cabal install opensrs`

## Installation for development

* Copy `tests/TestConfig.hs.example` to `tests/TestConfig.hs`
* Customise `tests/TestConfig.hs` to point to your development instance
* `cabal install --enable-tests`
