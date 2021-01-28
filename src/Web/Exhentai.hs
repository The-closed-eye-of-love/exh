module Web.Exhentai
  ( -- * Effects and effect interpreters

    -- ** Important
    Http,
    formRequest,
    genBoundary,
    respOpen,
    respClose,
    ConduitIO (..),
    runConduitIO,
    Cookie (..),
    takeCookie,
    readCookie,
    putCookie,
    ExhC,
    exhToIO,

    -- ** Unimportant
    HttpH,
    HttpToIOC,
    httpToIO,
    CookieH,
    CookieToIOC,
    cookieToIO,
    ConduitIOH,
    ConduitIOToIOC,
    conduitIOToIO,

    -- * API

    -- ** Authentication
    module Au,

    -- ** Getting information about galleries
    module G,

    -- ** Fetching images from galleries
    module M,

    -- ** Searching galleries
    module S,

    -- ** Getting popular and watched galleries
    module W,

    -- ** Fetching archives
    module Ar,

    -- * Errors
    ExhentaiError (..),
  )
where

import Control.Effect.Exh
import Web.Exhentai.API.Archiver as Ar
import Web.Exhentai.API.Auth as Au
import Web.Exhentai.API.Gallery as G
import Web.Exhentai.API.MPV as M
import Web.Exhentai.API.Search as S
import Web.Exhentai.API.Watched as W
import Web.Exhentai.Errors
