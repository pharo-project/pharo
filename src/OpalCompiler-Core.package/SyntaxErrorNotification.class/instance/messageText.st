accessing
messageText
	^ super messageText
		ifNil: [messageText := code]