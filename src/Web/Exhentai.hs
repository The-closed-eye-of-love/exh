module Web.Exhentai
  ( -- ** Authentication
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
  )
where

import Web.Exhentai.API.Archiver as Ar
import Web.Exhentai.API.Auth as Au
import Web.Exhentai.API.Gallery as G
import Web.Exhentai.API.MPV as M
import Web.Exhentai.API.Search as S
import Web.Exhentai.API.Watched as W
