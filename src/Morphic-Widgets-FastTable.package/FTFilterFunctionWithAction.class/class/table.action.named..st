instance creation
table: aFTTableMorph action: aBlockClosure named: aString
	^ (self table: aFTTableMorph)
		action: aBlockClosure named: aString;
		yourself