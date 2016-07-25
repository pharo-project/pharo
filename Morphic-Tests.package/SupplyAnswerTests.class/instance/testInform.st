tests
testInform

	| answer |
	[ MorphicUIManager new inform: 'blah' ] on: Exception do: [:ex | answer := ex messageText ].
	self assert: answer  = 'blah'.