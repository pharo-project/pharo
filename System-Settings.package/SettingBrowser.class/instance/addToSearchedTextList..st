accessing
addToSearchedTextList: aString
	(self searchedTextList includes: aString) 
		ifFalse: [self searchedTextList size > 5 ifTrue: [self searchedTextList removeFirst].
		self searchedTextList add: aString]