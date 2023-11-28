module Test.MySolutions where

import Prelude

import Data.AddressBook (Entry, AddressBook)
import Data.List (filter, head, nubByEq, null)
import Data.Maybe (Maybe)

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter filterEntryByStreet
  where
  filterEntryByStreet :: Entry -> Boolean
  filterEntryByStreet e = e.address.street == street 

isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName = not null <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry =
     entry.firstName == firstName &&
     entry.lastName  == lastName

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq findDuplicate
  where
    findDuplicate :: Entry -> Entry -> Boolean
    findDuplicate entry1 entry2 = 
      entry1.firstName == entry2.firstName && 
      entry1.lastName  == entry2.lastName