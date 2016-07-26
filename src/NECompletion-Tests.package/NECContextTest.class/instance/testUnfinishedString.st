tests
testUnfinishedString
	| text context |
	text := 'testIt: aRectangle
	| a b c |
	self test: ''test it and so'.
	context := self 
		createContextFor: text
		at: text size.
	self assert: context completionToken = 'so'