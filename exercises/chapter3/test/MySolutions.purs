module Test.MySolutions where

import Prelude

import Data.AddressBook (Entry, AddressBook)
import Data.List (filter, head, nubByEq, null)
import Data.Maybe (Maybe)

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter (\entry -> entry.address.street == street) {- ilterEntryByStreet -}
  {-
  where
  filterEntryByStreet :: Entry -> Boolean
  filterEntryByStreet entry = _.address.street entry == street 
  -}

isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName book = not $ null(filter (\entry -> entry.firstName == firstName && entry.lastName == lastName) {- filterEntry -} book)
  {-
  where
    filterEntry :: String -> String -> Entry -> Boolean
    filterEntry _ _ entry = _.firstName entry == firstName && _.lastName entry == lastName
  -}

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq {- findDuplicate -} (\entry1 entry2 -> entry1.firstName == entry2.firstName && entry1.lastName == entry2.lastName)
  {-
  where
    findDuplicate :: Entry -> Entry -> Boolean
    findDuplicate entry1 entry2 = entry1.firstName == entry2.firstName && entry1.lastName == entry2.lastName
  -}