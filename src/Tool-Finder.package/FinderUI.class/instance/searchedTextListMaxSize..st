accessing
searchedTextListMaxSize: anInteger
	[self searchedTextList size > anInteger]
		whileTrue: [ self searchedTextList removeLast].
	self changed: #searchedTextList.