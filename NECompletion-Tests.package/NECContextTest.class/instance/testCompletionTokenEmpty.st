tests
testCompletionTokenEmpty
	| text context |
	text := 'testIt: aRectangle
	| abc |
	test.
	abc daf '.
	context := self createContextFor: text at: text size.
	self assert: context completionToken = ''