-- $Id: HepTests.hs,v 1.2 2015/01/21 21:00:49 leavens Exp leavens $
module Hep where

hep :: [String] -> [String]

hep str = case str of
  [] -> []
  ("you":others) -> ("u": hep others)
  ("are":others) -> ("r": hep others)
  ("your":others) -> ("ur": hep others)
  ("by" :"the" :"way":others) -> ("btw": hep others)
  ("for":"your" :"information":others) -> ("fyi": hep others)
  ("boyfriend":others) -> ("bf": hep others)
  ("girlfriend":others) -> ("gf": hep others)
  ("be": "right" :"back":others) -> ("brb": hep others)
  ("laughing": "out" :"loud":others) -> ("lol": hep others)
  ("see":"you":others) -> ("cya": hep others)
  ("I":"will":others) -> ("I'll": hep others)
  ("great":others) -> ("gr8": hep others)
  (x :others) -> (x : hep others)


