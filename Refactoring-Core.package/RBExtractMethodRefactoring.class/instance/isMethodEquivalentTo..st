transforming
isMethodEquivalentTo: aSelector 
	selector == aSelector ifTrue: [^false].
	aSelector numArgs ~~ parameters size ifTrue: [^false].
	(self isParseTreeEquivalentTo: aSelector) ifFalse: [^false].
	self reorderParametersToMatch: aSelector.
	^true