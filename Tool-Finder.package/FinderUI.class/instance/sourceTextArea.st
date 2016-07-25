accessing
sourceTextArea
	^sourceTextArea ifNil: [self buildSourceTextArea]