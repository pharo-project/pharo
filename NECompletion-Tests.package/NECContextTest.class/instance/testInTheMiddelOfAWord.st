tests
testInTheMiddelOfAWord
	| text context |
	text := 'hagada'.
	context := self 
		createContextFor: text
		at: 4.
	self assert: context completionToken = 'haga'