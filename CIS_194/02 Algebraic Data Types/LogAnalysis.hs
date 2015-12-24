{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- CIS 194: Homework 2
-- Nicholas Yan
-- December 23, 2015
-- http://www.seas.upenn.edu/~cis194/spring13/hw/02-ADTs.pdf

-- EXERCISE 1

-- parses an individual message
-- e.g. parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"

parseMessage :: String -> LogMessage
parseMessage str = case (words str) of
    ("E":lvl:ts:msg) -> LogMessage (Error (read lvl)) (read ts) (unwords msg)
    ("I":ts:msg)     -> LogMessage Info (read ts) (unwords msg)
    ("W":ts:msg)     -> LogMessage Warning (read ts) (unwords msg)
    _                -> Unknown str  

-- parses the entire log file and then returns a list of LogMessages
parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- EXERCISE 2

-- insert a new LogMessage into an existing MessageTree
-- (if the LogMessage is Unknown, don't add it to the tree)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mtree                = mtree
insert msg (Leaf)                       = Node Leaf msg Leaf
insert msg@(LogMessage _ ts _) (Node left root@(LogMessage _ rts _) right)
    | ts < rts  = Node (insert msg left) root right
    | otherwise = Node left root (insert msg right)

-- unnecessary last case; an Unknown message should never be in the tree
-- (just to allow compilation w/o warnings)
insert _ root@(Node _ (Unknown _) _)    = root

-- EXERCISE 3

-- builds a complete MessageTree from a list of messages

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf 

-- EXERCISE 4

-- takes in a sorted MessageTree and prints a list of all of the messages that
-- the MessageTree contains

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                   = []
inOrder (Node left msg right)  = (inOrder left) ++ [msg] ++ (inOrder right)

-- EXERCISE 5

-- extracts all error messages with severity of at least 50

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = foldr insertSevere [] (inOrder (build msgs))

insertSevere :: LogMessage -> [String] -> [String]
insertSevere (LogMessage (Error lvl) _ msg) list
    | lvl >= 50 = [msg] ++ list
    | otherwise = [] ++ list
-- if we've reached this point, it must not be an error message
insertSevere _ list = [] ++ list

-- version utilizing filter function
-- inspired by: https://github.com/BerndSchwarzenbacher/cis194-solutions/blob/master/02-adt/LogAnalysis.hs
--          and https://github.com/prakashk/cis-194/blob/master/wk02/LogAnalysis.hs 

whatWentWrongFilter :: [LogMessage] -> [String]
whatWentWrongFilter = extractMsgs . inOrder . build . filter (isSevere 50)

isSevere :: Int -> LogMessage -> Bool
isSevere cutoff (LogMessage (Error lvl) _ _)
    | lvl >= cutoff = True
    | otherwise     = False
-- if we've reached this point, it must not be an error message
isSevere _ _        = False 

extractMsgs :: [LogMessage] -> [String]
extractMsgs msgs = foldr extractMsg [] msgs
    where extractMsg (LogMessage (Error _) _ msg) list = [msg] ++ list 
          extractMsg _ list = [] ++ list
