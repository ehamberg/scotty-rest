# scotty-rest

[![Build Status](https://travis-ci.org/ehamberg/scotty-rest.svg)](https://travis-ci.org/ehamberg/scotty-rest)
[![Coverage Status](https://coveralls.io/repos/ehamberg/scotty-rest/badge.svg?branch=master&service=github)](https://coveralls.io/github/ehamberg/scotty-rest?branch=master)

A Webmachine-style library for [scotty](https://github.com/scotty-web/scotty)
inspired by [cowboy](https://github.com/ninenines/cowboy).

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Network.Wai.Middleware.RequestLogger
import Web.Scotty.Rest
import Web.Scotty.Trans

main :: IO ()
main = scottyT 3000 id $ do
  middleware logStdoutDev
  rest "/" defaultConfig {
    contentTypesProvided = return [("text/html",html "Hello, World!")]
  }
```
