tests
testCopy
    | theOriginal theCopy |
    theOriginal := WideCharacterSet newFrom: ('abc' copyWith: 300 asCharacter).
    theCopy := theOriginal copy.
	 self assert: theCopy equals: theOriginal.
    theCopy remove: $a.
    ^self assert: (theOriginal includes: $a) 
				description: 'Changing the copy should not change the original'