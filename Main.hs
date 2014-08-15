{-
    hbot - a simple Haskell chat bot for Hipchat
    Copyright (C) 2014 Louis J. Scoras

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module Main where

--------------------------------------------------------------------------------
import           Control.Applicative       ( (<$>), (<*>) )
import           System.Environment        ( getEnv )

import           Hbot.Server

main :: IO ()
main = app =<< ps
  where
    ps = AppParams <$> fmap read (getEnv "PORT")
                   <*> getEnv "ROOM"
                   <*> getEnv "PREFIX"
                   <*> getEnv "AUTH_TOKEN"

