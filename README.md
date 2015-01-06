# OpenSRS API for Haskell

This is a Haskell library for interacting with the [OpenSRS][] wholesale domain
name registration API.

© 2014 [Anchor Systems][] Pty Ltd and Others

[![Build Status](https://travis-ci.org/anchor/haskell-opensrs.svg)](https://travis-ci.org/anchor/haskell-opensrs)

[OpenSRS]: https://opensrs.com/
[Anchor Systems]: http://www.anchor.com.au/

## Usage

```haskell
let config = SRSConfig "https://horizon.opensrs.net:55443" "myusername"
        "mykey" "127.0.0.1" False

res <- doRequest $ LookupDomain config "bigfatchunkybear.info"

res2 <- doRequest $ GetDomain config "hobbseetest.com"

res3 <- doRequest $ ModifyDomain config "im-a-little-teapot.com" False
        (fromList [("data", "whois_privacy_state"), ("state", "enable")])
        Nothing
```

### SRSConfig

The SRSConfig object takes five values:

* Endpoint URL
* Username
* Authentication Key
* Current IP address
* Boolean value indicating whether you want XML requests/responses to be printed to STDOUT or not

### General usage

When you're sending a request to OpenSRS, you generally do it in two steps:

1. Generate the `SRSRequest` object to send
2. Send it through `doRequest` to get a response

### Types of SRSRequest

The following types of `SRSRequest` are supported:

#### ListDomainsByExpiry

Lists domain by expiry date. Can be used to enumerate all domains in your reseller account.  
`ListDomainsByExpiry requestConfig requestStartDate requestEndDate requestPage requestLimit`

```haskell  
endTime <- getCurrentTime  
let startTime = addUTCTime ((86400 * 365 * -1) :: NominalDiffTime) endTime  
let req = ListDomainsByExpiry cfg startTime endTime 0 100  
```

#### GetDomain

Gets a domain by name.  
`GetDomain requestConfig requestDomainName`

```haskell  
let req = GetDomain cfg "foo.com"  
```

#### GetDomainWithCookie

If you have a cookie from a previous request, fetch a domain.  
`GetDomainWithCookie requestConfig requestDomainName requestCookie`  

```haskell  
let req = GetDomainWithCookie cfg "foo.com" cookieStr  
```

#### GetDomainTldData

Fetches TLD data for this domain.  
`GetDomainTldData requestConfig requestDomainName`  

```haskell  
let req = GetDomainTldData cfg "foo.com"  
```

#### LookupDomain

Checks if a given domain exists under your reseller account.  
`LookupDomain requestConfig requestDomainName`

```haskell  
let req = LookupDomain cfg "foo.com"  
```

#### RegisterDomain

Registers a new domain, or transfers an existing one.  
`RegisterDomain requestConfig requestDomain requestChangeContact requestComments requestEncoding requestLock requestPark requestWhoisPrivacy requestHandleNow requestPeriod requestUsername requestPassword requestRegType requestTldData`  

```haskell
let c = Contact (Just "Jane")
                    (Just "Doe")
                    (Just "Frobozz Pty Ltd")
                    (Just "geoffrey.roberts@anchor.com.au")
                    (Just "+61.299999999")
                    Nothing
                    (Just "Frobozz Pty Ltd")
                    (Just "Level 50")
                    (Just "1 George Street")
                    (Just "Sydney")
                    (Just "NSW")
                    (Just "2000")
                    (Just "AU")

let domainContacts = fromList [("owner", c),
                               ("admin", c),
                               ("billing", c),
                               ("tech", c)]

let nameservers = [ Nameserver (Just "ns1.anchor.net.au") (Just "0") (Just "127.0.0.1")
                  , Nameserver (Just "ns2.anchor.net.au") (Just "0") (Just "127.0.0.2") ]

t <- getCurrentTime
let t' = addUTCTime ((86400 * 365) :: NominalDiffTime) t

let domain = Domain dname True domainContacts (Just t) True (Just t) (Just "12345") True (Just t') nameservers
let changeContact = False
let comments      = Just "hi there"
let encoding      = Nothing
let lock          = False
let park          = False
let whois         = False
let handleNow     = True
let regPeriod     = 1
let username      = fromJust $ makeUsername "webmaster"
let password      = fromJust $ makePassword "newPassword123"
let regType       = NewRegistration
let tldData       = Nothing

let req = RegisterDomain cfg domain changeContact comments encoding lock park whois handleNow regPeriod username password regType tldData
```

#### ModifyDomain

Modifies particular domain parameters.  
See the [OpenSRS API documentation](https://opensrs.com/resources/documentation/) for more details on what can be modified using this call.  
`ModifyDomain requestConfig requestDomainName requestAffectLinked requestData requestTldData`  

```haskell
let req = ModifyDomain cfg "foo.com" False (fromList [("data", "whois_privacy_state"), ("state", "enable")]) Nothing
```

#### UpdateDomain
Use to update contact information and nameservers only.  
`UpdateDomain requestConfig requestDomain`  

```haskell
let newContact = Contact (Just "Jane")
                         (Just "Doe")
                         (Just "Frobozz Pty Ltd")
                         (Just "geoffrey.roberts@anchor.com.au")
                         (Just "+61.299999999")
                         Nothing
                         (Just "Frobozz Pty Ltd")
                         (Just "Level 50")
                         (Just "1 George Street")
                         (Just "Sydney")
                         (Just "NSW")
                         (Just "2000")
                         (Just "AU")
let newDomainContacts = fromList [("owner", c),
                                  ("admin", c),
                                  ("billing", c),
                                  ("tech", c)]
let domain' = domain { domainContactSet = newDomainContacts }
let req = UpdateDomain cfg domain'
```

#### RenewDomain

Renew a domain that you have control over.  
`RenewDomain requestConfig requestDomainName requestAutoRenew requestAffiliateID requestExpiryYear requestHandleNow requestPeriod`  

```haskell
let req = RenewDomain cfg "foo.com" False "12345" 2015 True 1
```

#### ChangeDomainOwnership

Change the username and password used to manage a domain. It will still be under the control of your reseller account, but this will change user credentials.  
`ChangeDomainOwnership requestConfig requestDomainName requestUsername requestPassword`  

```haskell
let username = fromJust $ makeUsername "webmaster"
let password = fromJust $ makeUsername "totesSecureEh"
let req = ChangeDomainOwnership cfg "foo.com" username password
```

#### SendDomainPassword

Send a domain password to its administrator or owner.  
`requestSendTo` must be either "admin" or "owner".  
`SendDomainPassword requestConfig requestDomainName requestSendTo requestToSubuser`  


```haskell
let req = SendDomainPassword cfg "foo.com" "owner" False
```

#### SetCookie

Gets a cookie for a given domain, using its username and password credentials to authenticate.  
`SetCookie requestConfig requestDomainName requestUsername requestPassword`  

```haskell
let username = fromJust $ makeUsername "webmaster"
let password = fromJust $ makeUsername "password123"
let req = SetCookie cfg "foo.com" username password
```

## Installation for usage

* `cabal install opensrs`

## Installation for development

* Copy `tests/TestConfig.hs.example` to `tests/TestConfig.hs`
* Customise `tests/TestConfig.hs` to point to your development instance
* `cabal install --enable-tests`
