tests
testReceiverArgument
	| text context |
	text := 'testIt: aRectangle
	aRectangle printS'.
	context := self 
		createContextFor: text
		at: text size.
	self assert: context receiverClass == Rectangle.
	text := 'testIt: rect
	rect is'.
	context := self 
		createContextFor: text
		at: text size.
	self assert: context receiverClass isNil