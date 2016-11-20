# scotty-rest

[![Build Status](https://travis-ci.org/ehamberg/scotty-rest.svg)](https://travis-ci.org/ehamberg/scotty-rest)
[![Coverage Status](https://coveralls.io/repos/ehamberg/scotty-rest/badge.svg?branch=master&service=github)](https://coveralls.io/github/ehamberg/scotty-rest?branch=master)

A Webmachine-style library for [scotty](https://github.com/scotty-web/scotty)
inspired by [cowboy](https://github.com/ninenines/cowboy)'s [cowboy_rest](http://ninenines.eu/docs/en/cowboy/HEAD/manual/cowboy_rest/) module.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Network.Wai.Middleware.RequestLogger
import Web.Scotty.Rest
import Web.Scotty.Trans

main :: IO ()
main = scottyT 3000 id $ do
  middleware logStdoutDev
  rest "/" defaultConfig {
    contentTypesProvided = return [("text/html", html "Hello, World!")]
  }

```

# Introduction

This library implements server-side HTTP semantics as illustrated by the [Webmachine diagram](https://github.com/webmachine/webmachine/wiki/Diagram). Instead of simply having handlers for endpoints that then have to take full responsibility for handling all the behaviour expected from an HTTP server (using the correct return code, including the correct headers that go with some return codes, etc.), you implement the “decision nodes” and get correct semantics “for free”. These decision nodes are regular functions and they all have default implementations that do what you expect.

Some examples of “decision functions” and their default implementations:

- `serviceAvailable`: `return True`
- `isAuthorized`: `return Authorized`
- `charSetsProvided`: `Nothing` (i.e. ignore `Accept-Charsets` header) 
- `allowedMethods`: `[GET, HEAD, OPTIONS]`
- `resourceMoved`: `NotMoved`

# Usage

The two functions that you almost always is going to define yourself is `contentTypesProvided` and/or `contentTypesAccepted`. These are both a list of pairs of content types (e.g. `application/json`) together with a *handler* for that type. A *handler* is a function that is responsible for producing a response for its associated content type (in the case of `contentTypesProvided`) or consuming a value of its associated content type (in the case of `contentTypesAccepted`).

In the example above, the only function overridden is `contentTypesProvided`. Our implementations says that we provide only one content type (`text/html`) and if someone asks for `text/html` (or `*/*`) we will use `Scotty's` `html` function to return the text "Hello World".

Some example interactions with this server:

## Requesting any content type

```
GET / HTTP/1.1
Accept: */*
Host: localhost:3000
```

```
HTTP/1.1 200 OK
Date: Sat, 19 Nov 2016 14:32:20 GMT
Server: Warp/3.2.9
Transfer-Encoding: chunked
Content-Type: text/html

Hello, World!
```

## Requesting a content type that isn't provided

```
GET / HTTP/1.1
Accept: application/json
Host: localhost:3000
```

```
HTTP/1.1 406 Not Acceptable
Date: Sat, 19 Nov 2016 14:32:30 GMT
Server: Warp/3.2.9
Transfer-Encoding: chunked
```

## `POST`ing a JSON document

```
POST / HTTP/1.1
Accept: application/json, */*
Content-Length: 13
Content-Type: application/json
Host: localhost:3000

{
    "test": "1"
}
```

```
HTTP/1.1 405 Method Not Allowed
Date: Sat, 19 Nov 2016 14:32:57 GMT
Server: Warp/3.2.9
Transfer-Encoding: chunked
Allow: GET, HEAD, OPTIONS
```

# Flow diagrams

The diagrams below show the flow of how `scotty-rest` will handle a request and the functions that are called at each decision point.


                            Start

                              │
                              │
                              ▼
          ╔══════════════════════════════════════╗      False        ┌────────────────────────────┐
          ║           serviceAvailable           ║──────────────────▶│503 Service Unavailable     │
          ╚══════════════════════════════════════╝                   └────────────────────────────┘
                              │
                              │ True
                              │
                              ▼
          ╔══════════════════════════════════════╗     False         ┌────────────────────────────┐
          ║        method ∈ knownMethods         ║──────────────────▶│501 Not Implemented         │
          ╚══════════════════════════════════════╝                   └────────────────────────────┘
                              │
                              │ True
                              │
                              ▼
          ╔══════════════════════════════════════╗       True        ┌────────────────────────────┐
          ║              uriTooLong              ║──────────────────▶│414 Request URI Too Long    │
          ╚══════════════════════════════════════╝                   └────────────────────────────┘
                              │
                              │ False
                              │
                              ▼
          ╔══════════════════════════════════════╗      False        ┌────────────────────────────┐
          ║       method ∈ allowedMethods        ║──────────────────▶│405 Method Not Allowed      │
          ╚══════════════════════════════════════╝                   └────────────────────────────┘
                              │
                              │  True
                              │
                              ▼
          ╔══════════════════════════════════════╗      True         ┌────────────────────────────┐
          ║           malformedRequest           ║──────────────────▶│400 Bad Request             │
          ╚══════════════════════════════════════╝                   └────────────────────────────┘
                              │
                              │ False
                              │
                              ▼
          ╔══════════════════════════════════════╗   NotAuthorized   ┌────────────────────────────┐
          ║             isAuthorized             ║──────────────────▶│401 Unauthorized            │
          ╚══════════════════════════════════════╝                   └────────────────────────────┘
                              │
                              │ Authorized
                              │
                              ▼
          ╔══════════════════════════════════════╗       True        ┌────────────────────────────┐
          ║              forbidden               ║──────────────────▶│403 Forbidden               │
          ╚══════════════════════════════════════╝                   └────────────────────────────┘
                              │
                              │ True
                              │
                              ▼
          ╔══════════════════════════════════════╗      False        ┌────────────────────────────┐
          ║         validContentHeaders          ║──────────────────▶│501 Not Implemented         │
          ╚══════════════════════════════════════╝                   └────────────────────────────┘
                              │
                              │ True
                              │
                              ▼
          ╔══════════════════════════════════════╗      False        ┌────────────────────────────┐
          ║          validEntityLength           ║──────────────────▶│413 Request Entity Too Large│
          ╚══════════════════════════════════════╝                   └────────────────────────────┘
                              │
                              │
                              │                                   ┌─────────────────────────────────┐
                              ▼                                   │                          OPTIONS│
                  ┏━━━━━━━━━━━━━━━━━━━━━━┓                        │                                 │
                  ┃                      ┃            Yes         │  ╔════════════════════════════╗ │
                  ┃  Method is OPTIONS?  ┃────────────────────────┼─▶║          options           ║ │
                  ┃                      ┃                        │  ╚════════════════════════════╝ │
                  ┗━━━━━━━━━━━━━━━━━━━━━━┛                        │                 │               │
                              │                                   │                 │               │
                              │                                   │                 ▼               │
                              │                                   │  ┌────────────────────────────┐ │
                              │                                   │  │           200 OK           │ │
                              │                                   │  └────────────────────────────┘ │
                              │                                   │                                 │
                              │                                   │                                 │
                              │                                   └─────────────────────────────────┘
                              │
    ┌─────────────────────────┼─────────────────────────────────────────────────────────────────────┐
    │                         ▼                                                  Content negotiation│
    │             ┏━━━━━━━━━━━━━━━━━━━━━━┓                                                          │
    │      No     ┃                      ┃                                                          │
    │  ┌──────────┃ Has “Accept” Header? ┃                                                          │
    │  │          ┃                      ┃                                                          │
    │  │          ┗━━━━━━━━━━━━━━━━━━━━━━┛                                                          │
    │  │                      │                                                                     │
    │  │                      │ Yes                                                                 │
    │  │                      ▼                                       Not                           │
    │  │  ╔══════════════════════════════════════╗                  Provided                        │
    │  │  ║         contentTypesProvided         ║────────────────────────────────┐                 │
    │  │  ╚══════════════════════════════════════╝                                │                 │
    │  │                      │                                                   │                 │
    │  │                      │ Provided                                          │                 │
    │  │                      ▼                                                   │                 │
    │  │          ┏━━━━━━━━━━━━━━━━━━━━━━┓                                        │                 │
    │  └─────────▶┃Has “Accept-Language” ┃                                        │                 │
    │             ┃       Header?        ┃                                        │                 │
    │  ┌──────────┃                      ┃                                        │                 │
    │  │    No    ┗━━━━━━━━━━━━━━━━━━━━━━┛                                        │                 │
    │  │                      │                                                   │                 │
    │  │                      │ Yes                                               │                 │
    │  │                      ▼                           Not                     ▼                 │
    │  │  ╔══════════════════════════════════════╗      provided        ┌───────────────────┐       │
    │  │  ║          languagesProvided           ║─────────────────────▶│406 Not Acceptable │       │
    │  │  ╚══════════════════════════════════════╝                      └───────────────────┘       │
    │  │                      │                                                   ▲                 │
    │  │                      │ Provided                                          │                 │
    │  │                      ▼                                                   │                 │
    │  │          ┏━━━━━━━━━━━━━━━━━━━━━━┓                                        │                 │
    │  └─────────▶┃ Has “Accept-Charset” ┃                                        │                 │
    │             ┃       Header?        ┃                                        │                 │
    │  ┌──────────┃                      ┃                                        │                 │
    │  │    No    ┗━━━━━━━━━━━━━━━━━━━━━━┛                                        │                 │
    │  │                      │                                                   │                 │
    │  │                      │ Yes                                               │                 │
    │  │                      ▼                                    Not            │                 │
    │  │  ╔══════════════════════════════════════╗               provided         │                 │
    │  │  ║           charSetsProvided           ║────────────────────────────────┘                 │
    │  │  ╚══════════════════════════════════════╝                                                  │
    │  │                      │                                                                     │
    │  │                      │ Provided                                                            │
    │  │                      ▼                                                                     │
    │  │  ╔══════════════════════════════════════╗                                                  │
    │  └─▶║              variances               ║                                                  │
    │     ╚══════════════════════════════════════╝                                                  │
    │                         │                                                                     │
    └─────────────────────────┼─────────────────────────────────────────────────────────────────────┘
                              │
                              │
                              │
                              ▼

           See GET/HEAD, PUT/POST/PATCH or DELETE
             diagram, depending on the method.


## `GET`/`HEAD` requests

                   Continuing from
                Content Negotiation…

                          │
                          │
    ┌─────────────────────┼─────────────────────────────────────────────────────────────────────────┐
    │                     │                                                                 GET/HEAD│
    │                     ▼                                                                         │
    │ ╔══════════════════════════════════════╗            False                                     │
    │ ║            resourceExists            ║────────────────────────────┐                         │
    │ ╚══════════════════════════════════════╝                            │                         │
    │                     │                                               │                         │
    │                     │                                               │                         │
    │                     │ True                                          │                         │
    │                     │                                               ▼                         │
    │                     ▼                                   ┏━━━━━━━━━━━━━━━━━━━━━━┓              │
    │                                                         ┃                      ┃  False       │
    │    See “Conditional requests” diagram                   ┃Has “If-Match” Header?┃───────┐      │
    │                                                         ┃                      ┃       │      │
    │                     │                                   ┗━━━━━━━━━━━━━━━━━━━━━━┛       │      │
    │                     │                                               │                  │      │
    │                     ▼                                               │ True             │      │
    │    ╔════════════════════════════════╗                               ▼                  │      │
    │    ║          generateEtag          ║                   ┌──────────────────────┐       │      │
    │    ╚════════════════════════════════╝                   │ 412 Precond. Failed  │       │      │
    │                     │                                   └──────────────────────┘       │      │
    │                     │                                                                  │      │
    │                     ▼                                                                  │      │
    │    ╔════════════════════════════════╗                                                  │      │
    │    ║          lastModified          ║              ╔════════════════════════════════╗  │      │
    │    ╚════════════════════════════════╝           ┌──║       previouslyExisted        ║◀─┘      │
    │                     │                           │  ╚════════════════════════════════╝         │
    │                     │                           │                   │                         │
    │                     ▼                           │ True              │ False                   │
    │    ╔════════════════════════════════╗           │                   ▼                         │
    │    ║            expires             ║           │       ┌──────────────────────┐              │
    │    ╚════════════════════════════════╝           │       │    404 Not Found     │              │
    │                     │                           │       └──────────────────────┘              │
    │                     │                           │                                             │
    │                     ▼                           │                                             │
    │    ╔════════════════════════════════╗           │                                             │
    │    ║                                ║           │  ╔════════════════════════════════╗         │
    │    ║  Run callback for negotiated   ║           └─▶║             moved              ║         │
    │    ║   content type from list in    ║              ╚════════════════════════════════╝         │
    │    ║      contentTypesProvided      ║                               │                         │
    │    ║                                ║                               │                         │
    │    ╚════════════════════════════════╝                  ┌────────────┼─────────────┐           │
    │                     │                                  │            │             │           │
    │                     │                                  │            │             │           │
    │                     ▼                                  │            │             │           │
    │    ╔════════════════════════════════╗          MovedTemporarily     │      MovedPermantly     │
    │ ┌──║        multipleChoices         ║                  │            │             │           │
    │ │  ╚════════════════════════════════╝                  │            │             │           │
    │ │                   │                                  │        NotMoved          │           │
    │ │                   │                                  │            │             │           │
    │ │ Multiple*         │ UniqueRepresentation             │            │             │           │
    │ │                   │                                  ▼            │             ▼           │
    │ │                   ▼                      ┌──────────────────────┐ │ ┌──────────────────────┐│
    │ │       ┌──────────────────────┐           │307 Moved Temporarily │ │ │301 Moved Permanently ││
    │ │       │        200 OK        │           └──────────────────────┘ │ └──────────────────────┘│
    │ │       └──────────────────────┘                                    │                         │
    │ │                                                                   ▼                         │
    │ │       ┌──────────────────────┐                        ┌──────────────────────┐              │
    │ └──────▶│ 300 Multiple Choices │                        │       410 Gone       │              │
    │         └──────────────────────┘                        └──────────────────────┘              │
    │                                                                                               │
    └───────────────────────────────────────────────────────────────────────────────────────────────┘


## `PUT`/`POST`/`PATCH` requests


                     Continuing from
                  Content Negotiation…

                            │
                            │
    ┌───────────────────────┼───────────────────────────────────────────────────────────────────────┐
    │                       │                                                         PUT/POST/PATCH│
    │                       ▼                                                                       │
    │       ╔═══════════════════════════════╗               False                                   │
    │       ║        resourceExists         ║───────────────────────────────┐                       │
    │       ╚═══════════════════════════════╝                               │                       │
    │                       │                                               │                       │
    │                       │                                               │                       │
    │                       │ True                                          │                       │
    │                       │                                               ▼                       │
    │                       ▼                                   ┏━━━━━━━━━━━━━━━━━━━━━━┓            │
    │                                                           ┃                      ┃  False     │
    │      See “Conditional requests” diagram                   ┃Has “If-Match” Header?┃─────────┐  │
    │                                                           ┃                      ┃         │  │
    │                       │                                   ┗━━━━━━━━━━━━━━━━━━━━━━┛         │  │
    │                       │                                               │                    │  │
    │                       │                                               │ True               │  │
    │                       │                                               ▼                    │  │
    │ ┌─────────────────────┘                                   ┌──────────────────────┐         │  │
    │ │                                                         │ 412 Precond. Failed  │         │  │
    │ │                                                         └──────────────────────┘         │  │
    │ │                                                                                          │  │
    │ │                                                                                          │  │
    │ │                                                                                          │  │
    │ │                                                         ┏━━━━━━━━━━━━━━━━━━━━━━┓         │  │
    │ │                                               False     ┃                      ┃         │  │
    │ │  ┌──────────────────────────────────────────────────────┃Method is POST/PATCH? ┃◀────────┘  │
    │ │  │                                                      ┃                      ┃            │
    │ │  │                                                      ┗━━━━━━━━━━━━━━━━━━━━━━┛            │
    │ │  │                                                                  │                       │
    │ │  │                                                                  │ True                  │
    │ │  │           ┏━━━━━━━━━━━━━━━━━━━━━━┓                               ▼                       │
    │ │  │           ┃                      ┃       False  ╔════════════════════════════════╗       │
    │ │  │           ┃   Method is POST?    ┃◀─────────────║       previouslyExisted        ║       │
    │ │  │           ┃                      ┃              ╚════════════════════════════════╝       │
    │ │  │           ┗━━━━━━━━━━━━━━━━━━━┳━━┛                               │                       │
    │ │  │                       │       │                                  │                       │
    │ │  │                  True │       │ False                            │                       │
    │ │  │                       ▼       └──────────┐                       │                       │
    │ │  │       ╔═══════════════════════════════╗  │                       │ True                  │
    │ │  │   ┌───║       allowMissingPost        ║  │                       │                       │
    │ │  │   │   ╚═══════════════════════════════╝  │                       │                       │
    │ │  │   │                   │                  │                       │                       │
    │ │  │   │ True              │ False            │                       ▼                       │
    │ │  │   │                   ▼                  │      ╔════════════════════════════════╗       │
    │ │  │   │       ┌──────────────────────┐       │      ║             moved              ║       │
    │ │  │   │       │    404 Not Found     │◀──────┘      ╚════════════════════════════════╝       │
    │ │  │   │       └──────────────────────┘                               │                       │
    │ │  │   │                                                              │                       │
    │ │  │   │                                                 ┌────────────┼─────────────┐         │
    │ │  │   │                                                 │            │             │         │
    │ │  │   │                                                 │            │             │         │
    │ │  │   └──────────────┐                                  │            │             │         │
    │ │  │                  │                                  │            │             │         │
    │ │  │                  ▼                          MovedTemporarily     │      MovedPermantly   │
    │ │  │      ┏━━━━━━━━━━━━━━━━━━━━━━┓                       │            │             │         │
    │ │  └─────▶┃                      ┃                       │        NotMoved          │         │
    │ │         ┃    Method is PUT?    ┃                       │            │             │         │
    │ └────────▶┃                      ┃                       │            │             │         │
    │           ┗━━━━━━━━━━━━━━━━━━━━━━┛                       ▼            │             ▼         │
    │                     │ │                      ┌──────────────────────┐ │ ┌─────────────────────┴┐
    │             False   │ │ True                 │307 Moved Temporarily │ │ │301 Moved Permanently │
    │    ┌────────────────┘ │                      └──────────────────────┘ │ └─────────────────────┬┘
    │    │                  │                                               │                       │
    │    │                  │                                               │                       │
    │    │                  │                                               ▼                       │
    │    │                  │                                   ┏━━━━━━━━━━━━━━━━━━━━━━┓            │
    │    │                  │                                   ┃                      ┃            │
    │    │                  │                                   ┃   Method is POST?    ┃            │
    │    │                  │                                   ┃                      ┃            │
    │    │                  │                                   ┗━━━━━━━━━━━━━━━━━━━━━━┛            │
    │    │                  │                                               │                       │
    │    │                  │                                               │ True                  │
    │    │                  ▼                                               ▼                       │
    │    │      ╔══════════════════════╗     False          ╔═══════════════════════════════╗       │
    │    │      ║      isConflict      ║─────────────┐      ║       allowMissingPost        ╠─────┐ │
    │    │      ╚══════════════════════╝             │      ╚═══════════════════════════════╝     │ │
    │    │                  │                        │                      │                     │ │
    │    │                  │  True                  │                      │ False          True │ │
    │    │                  ▼                        │                      ▼                     │ │
    │    │      ┌──────────────────────┐             │          ┌──────────────────────┐          │ │
    │    │      │     409 Conflict     │             │          │       410 Gone       │          │ │
    │    │      └──────────────────────┘             │          └──────────────────────┘          │ │
    │    │                                           │                                            │ │
    │    │                                           │                                            │ │
    │    │                                           │                                            │ │
    │    │                                           ▼                                            │ │
    │    │                            ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓                              │ │
    │    │                            ┃    Handler for type in     ┃                              │ │
    │    └───────────────────────────▶┃“Content-Type” header exists┃◀─────────────────────────────┘ │
    │                                 ┃  in contentTypesAccepted   ┃                                │
    │                                 ┗━━━━━━━━━┳━━━━━━━━┳━━━━━━━━━┛                                │
    │                                 Yes       │        │       No                                 │
    │                      ┌────────────────────┘        └────────────────────────┐                 │
    │                      │                                                      │                 │
    │                      ▼                                                      ▼                 │
    │     ╔════════════════════════════════╗                        ┌───────────────────────────┐   │
    │     ║ Run the callback function from ║    Failed              │415 Unsupported Media Type │   │
    │     ║  contentTypesAccepted for the  ║────────────┐           └───────────────────────────┘   │
    │     ║   content type we were given   ║            │                                           │
    │     ╚═══════╦════════════════╦════╦══╝            │                                           │
    │             │        │       │    │               │                                           │
    │             │        │       │    │               │            ┌───────────────────────────┐  │
    │             │        │       │    │               └───────────▶│      400 Bad Request      │  │
    │             │        │       │    │                            └───────────────────────────┘  │
    │             │        │       │    │   Redirect                                                │
    │             │        │       │    └───────────────┐                                           │
    │             │        │       │                    │                                           │
    │             │        │       │                    │            ┌───────────────────────────┐  │
    │             │        │       └───┐                └───────────▶│       303 See Other       │  │
    │             │        │           │                             └───────────────────────────┘  │
    │             │        │           │                                                            │
    │             │        │           │                                                            │
    │             │        │           │                                                            │
    │        ┌────┘        │      Succeeded                                                         │
    │        │             │           │                                                            │
    │        │       ┌─────┘           │                                                            │
    │        │       │                 │                       False  ┌───────────────────────────┐ │
    │        │       │                 │                     ┌───────▶│        201 Created        │ │
    │        │       │                 ▼                     │        └───────────────────────────┘ │
    │        │       │  ╔════════════════════════════╗       │                                      │
    │        │       │  ║       resourceExists       ║───────┤                                      │
    │        │       │  ╚════════════════════════════╝       │                                      │
    │        │       │                                       │        ┌───────────────────────────┐ │
    │        │       │                                       └───────▶│      204 No Content       │ │
    │        │       │                                          True  └───────────────────────────┘ │
    │        │       │                                                                              │
    │        │   SuccededWithContent                                                                │
    │        │       │                                         False  ┌───────────────────────────┐ │
    │        │       │                                       ┌───────▶│        201 Created        │ │
    │        │       │                                       │        └───────────────────────────┘ │
    │        │       │  ╔════════════════════════════╗       │                                      │
    │        │       └─▶║       resourceExists       ║───────┤                                      │
    │        │          ╚════════════════════════════╝       │                                      │
    │        │                                               │         ╔═══════════════════╗        │
    │        │                                               └────────▶║  multipleChoices  ║───┐    │
    │  SuccededWithLocation                                     True   ╚═══════════════════╝   │    │
    │        │                                                                   │             │    │
    │        │                                                                   │             │    │
    │        │          ╔════════════════════════════╗                UniqueRepresentation     │    │
    │        └─────────▶║       resourceExists       ║                           │             │    │
    │                   ╚═══════╦═════════════╦══════╝                           │             │    │
    │                           │             │                                  │        Multiple* │
    │                    True   │             │False                             ▼             │    │
    │               ┌───────────┘             └─────┐                ┌──────────────────────┐  │    │
    │               │                               │                │        200 OK        │  │    │
    │               │                               │                └──────────────────────┘  │    │
    │               │                               │                                          │    │
    │               ▼                               ▼                ┌──────────────────────┐  │    │
    │ ┌───────────────────────────┐   ┌───────────────────────────┐  │ 300 Multiple Choices │◀─┘    │
    │ │      204 No Content       │   │        201 Created        │  └──────────────────────┘       │
    │ │    + “Location” header    │   │    + “Location” header    │                                 │
    │ └───────────────────────────┘   └───────────────────────────┘                                 │
    └───────────────────────────────────────────────────────────────────────────────────────────────┘


## `DELETE` requests

                   Continuing from
                Content Negotiation…

                          │
                          │
                          │
    ┌─────────────────────┼─────────────────────────────────────────────────────────────────────────┐
    │                     ▼                                                                   DELETE│
    │ ╔══════════════════════════════════════╗                                                      │
    │ ║            resourceExists            ║────────────────────────────┐                         │
    │ ╚══════════════════════════════════════╝                            │                         │
    │                     │                                               │                         │
    │                     │                                               │                         │
    │                     │                                               │                         │
    │                     │                                               ▼                         │
    │                     ▼                                   ┏━━━━━━━━━━━━━━━━━━━━━━┓              │
    │                                                         ┃                      ┃              │
    │    See “Conditional requests” diagram                   ┃Has “If-Match” Header?┃───────┐      │
    │                                                         ┃                      ┃       │      │
    │                     │                                   ┗━━━━━━━━━━━━━━━━━━━━━━┛       │      │
    │                     │                                               │                  │      │
    │                     │                                               │                  │      │
    │                     ▼                                               ▼                  │      │
    │    ╔════════════════════════════════╗                   ┌──────────────────────┐       │      │
    │    ║         deleteResource         ║                   │ 412 Precond. Failed  │       │      │
    │    ╚════════════════════════════════╝                   └──────────────────────┘       │      │
    │        │    │              │    │                                                      │      │
    │        │    │              │    │                                                      │      │
    │        │    │         Deleted   │ DeleteEnacted                                        │      │
    │    ┌───┘    │              │    │                  ╔════════════════════════════════╗  │      │
    │    │    NotDeleted         │    │               ┌──║       previouslyExisted        ║◀─┘      │
    │    │        │    ┌─────────┘    │               │  ╚════════════════════════════════╝         │
    │    │        │    │              │               │                   │                         │
    │    │        │    │              ▼               │                   │ False                   │
    │    │        │    │  ┌──────────────────────┐    │                   ▼                         │
    │    │        │    │  │     202 Accepted     │    │  True ┌──────────────────────┐              │
    │    │        │    │  └──────────────────────┘    │       │    404 Not Found     │              │
    │    │        │    │                              │       └──────────────────────┘              │
    │    │        │    │  ┌──────────────────────┐    │                                             │
    │    │        │    └─▶│    204 No Content    │    │                                             │
    │    │        │       └──────────────────────┘    │                                             │
    │    │        │                                   │  ╔════════════════════════════════╗         │
    │    │        │       ┌──────────────────────┐    └─▶║             moved              ║         │
    │    │        └──────▶│      500 Error       │       ╚════════════════════════════════╝         │
    │    │                └──────────────────────┘                        │                         │
    │    └─────────────────┐                                              │                         │
    │                      │                                 ┌────────────┼─────────────┐           │
    │                      │                                 │            │             │           │
    │                      │                                 │            │             │           │
    │                      │                                 │            │             │           │
    │                      │                                 │            │             │           │
    │             DeletedWithResponse                MovedTemporarily     │      MovedPermantly     │
    │                      │                                 │            │             │           │
    │                      │                                 │        NotMoved          │           │
    │                      │                                 │            │             │           │
    │                      │                                 │            │             │           │
    │                      ▼                                 ▼            │             ▼           │
    │     ╔════════════════════════════════╗     ┌──────────────────────┐ │ ┌──────────────────────┐│
    │  ┌──║        multipleChoices         ║     │307 Moved Temporarily │ │ │301 Moved Permanently ││
    │  │  ╚════════════════════════════════╝     └──────────────────────┘ │ └──────────────────────┘│
    │  │                   │                                              │                         │
    │  │                   │                                              ▼                         │
    │  │ Multiple*         │ UniqueRepresentation             ┌──────────────────────┐              │
    │  │                   │                                  │       410 Gone       │              │
    │  │                   ▼                                  └──────────────────────┘              │
    │  │       ┌──────────────────────┐                                                             │
    │  │       │        200 OK        │                                                             │
    │  │       └──────────────────────┘                                                             │
    │  │                                                                                            │
    │  │       ┌──────────────────────┐                                                             │
    │  └──────▶│ 300 Multiple Choices │                                                             │
    │          └──────────────────────┘                                                             │
    └───────────────────────────────────────────────────────────────────────────────────────────────┘


## Conditional requests

    ┌───────────────────────────────────────────────────────────────────────────────────────────────┐
    │                                                                           Conditional requests│
    │                      ┏━━━━━━━━━━━━━━━━━━━━━━┓                                                 │
    │                      ┃                      ┃                                                 │
    │              ┌───────┃Has “If-Match” Header?┃                                                 │
    │              │       ┃                      ┃                                                 │
    │              │       ┗━━━━━━━━━━━━━━━━━━━━━━┛                                                 │
    │              │                   │                                                            │
    │              │                   │ True                                                       │
    │              │                   ▼                                                            │
    │              │  ╔════════════════════════════════╗      tags didn't match                     │
    │         False│  ║          generateEtag          ║───────────────────────────┐                │
    │              │  ╚════════════════════════════════╝                           │                │
    │              │                   │                                           │                │
    │              │                   │ tags matched                              │                │
    │              │                   ▼                                           │                │
    │              │       ┏━━━━━━━━━━━━━━━━━━━━━━┓                                │                │
    │              └──────▶┃         Has          ┃                                │                │
    │                      ┃“If-Unmodified-Since” ┃                                │                │
    │              ┌───────┃       Header?        ┃                                │                │
    │              │       ┗━━━━━━━━━━━━━━━━━━━━━━┛                                │                │
    │              │                   │                                           │                │
    │              │                   │ True                                      │                │
    │              │                   ▼                                           ▼                │
    │              │  ╔════════════════════════════════╗    modfied    ┌──────────────────────┐     │
    │        False │  ║          lastModified          ║──────────────▶│ 412 Precond. Failed  │     │
    │              │  ╚════════════════════════════════╝               └──────────────────────┘     │
    │              │                   │                                           ▲                │
    │              │                   │ not modified                              │                │
    │              │                   ▼                                           │                │
    │              │       ┏━━━━━━━━━━━━━━━━━━━━━━┓                                │                │
    │              └──────▶┃ Has “If-None-Match”  ┃                                │                │
    │                      ┃       Header?        ┃                                │ Yes            │
    │              ┌───────┃                      ┃                                │                │
    │              │       ┗━━━━━━━━━━━━━━━━━━━━━━┛                                │                │
    │              │                   │                                           │                │
    │              │                   │ True                                      │                │
    │              │                   ▼                               ┏━━━━━━━━━━━━━━━━━━━━━━┓     │
    │              │  ╔════════════════════════════════╗  tags matched ┃                      ┃     │
    │        False │  ║          generateEtag          ║──────────────▶┃ Method is GET/HEAD?  ┃     │
    │              │  ╚════════════════════════════════╝               ┃                      ┃     │
    │              │                   │                               ┗━━━━━━━━━━━━━━━━━━━━━━┛     │
    │              │                   │ True                                      │                │
    │              │                   ▼                                           │                │
    │              │       ┏━━━━━━━━━━━━━━━━━━━━━━┓                                │                │
    │              └──────▶┃         Has          ┃                                │                │
    │                      ┃ “If-Modified-Since”  ┃                                │ No             │
    │              ┌───────┃       Header?        ┃                                │                │
    │              │       ┗━━━━━━━━━━━━━━━━━━━━━━┛                                │                │
    │              │                   │                                           │                │
    │              │                   │ True                                      │                │
    │              │                   ▼                   not                     ▼                │
    │              │  ╔════════════════════════════════╗ modified ╔════════════════════════════════╗│
    │        False │  ║          lastModified          ║─────────▶║          generateEtag          ║│
    │              │  ╚════════════════════════════════╝          ╚════════════════════════════════╝│
    │              │                   │                                           │                │
    │              │                   │ modified                                  │                │
    │              └─────────────────┐ │                                           ▼                │
    │                                │ │                          ╔════════════════════════════════╗│
    │                                │ │                          ║            expires             ║│
    │                                │ │                          ╚════════════════════════════════╝│
    │                                │ │                                           │                │
    │                                │ │                                           │                │
    │                                │ │                                           ▼                │
    │                                │ │                               ┌──────────────────────┐     │
    │                                │ │                               │   304 Not Modified   │     │
    │                                │ │                               └──────────────────────┘     │
    └────────────────────────────────┼─┼────────────────────────────────────────────────────────────┘
                                     │ │
                                     │ │
                                     ▼ ▼
