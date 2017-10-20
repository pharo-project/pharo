tests
testCopy
    | theOriginal theCopy |
    theOriginal := WideCharacterSet newFrom: ('abc' copyWith: 300 asCharacter).
    theCopy := theOriginal copy.
    theCopy remove: $a.
    ^self should: [theOriginal includes: $a] description: 'Changing the copy should not change the original'.