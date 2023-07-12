module Feed where

import Hakyll.Web.Feed

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "James Leslie's Blog"
    , feedDescription = "This feed provides my blog posts!"
    , feedAuthorName  = "James Leslie"
    , feedAuthorEmail = "jamesleslie@posteo.net"
    , feedRoot        = "https://jeslie0.github.com"
    }
