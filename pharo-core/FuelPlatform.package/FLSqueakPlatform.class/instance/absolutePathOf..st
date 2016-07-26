accessing-system
absolutePathOf: aString
	^ (((Smalltalk globals at: #FileDirectory) forFileName: aString) / aString) fullName 