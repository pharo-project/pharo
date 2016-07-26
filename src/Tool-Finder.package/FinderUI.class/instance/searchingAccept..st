text areas behavior
searchingAccept: aText

	self forceSearch
		ifFalse: [aText asString = self searchingString ifTrue: [^self]].
	self searchingString: aText asString.
	forceSearch := false.