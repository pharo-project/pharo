*CodeImport
changeFromString: aString 
	"Parse the argument, aString, and make this be the receiver's structure."

	| categorySpecs |
	categorySpecs := aString parseLiterals.
	
	"If nothing was scanned and I had no elements before, then default me"
	(categorySpecs isEmpty and: [ self protocolOrganizer protocols isEmpty ])
		ifTrue: [ ^ self protocolOrganizer reset ].

	^ self changeFromCategorySpecs: categorySpecs