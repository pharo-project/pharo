accessing
searchingTextArea
	^searchingTextArea ifNil: [self buildSearchingTextArea]