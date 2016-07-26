method accessing
existingMethodsThatReferTo: aSymbol 
	| existingMethods |
	existingMethods := self realClass thoroughWhichSelectorsReferTo: aSymbol.
	(newMethods isNil and: [ removedMethods isNil ])
		ifTrue: [ ^ existingMethods ].
	^ existingMethods 
		reject: [ :each | (self hasRemoved: each) or: [ self newMethods includesKey: each ] ]