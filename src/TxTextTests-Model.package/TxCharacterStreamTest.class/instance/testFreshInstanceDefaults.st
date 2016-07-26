tests
testFreshInstanceDefaults

	| text stream |
	
	text := 'asdf' asTxModel.
	stream := text characterStream.
	
	self assert: stream text == text.
	self assert: stream position isAtStart.