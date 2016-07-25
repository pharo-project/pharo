accessing
addToSearchedTextList: aString 
	self searchedTextList: (self searchedTextList
			remove: aString
			ifAbsent: [];
			 yourself).
	self searchedTextList size = self searchedTextListMaxSize
		ifTrue: [self searchedTextList removeLast].
	self searchedTextList addFirst: aString 