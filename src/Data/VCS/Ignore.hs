-- |
-- Module      : Data.VCS.Ignore
-- Description : Git ignore queries and controllable repository traversal
-- Copyright   : (c) 2020-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- The library uses one lazy repository session for both individual ignore
-- queries and recursive traversal. Opening a repository does not scan its
-- working tree; per-directory rules are loaded only when a visible branch is
-- queried or traversed.
module Data.VCS.Ignore (
    module Data.VCS.Ignore.Git,
) where

import Data.VCS.Ignore.Git
