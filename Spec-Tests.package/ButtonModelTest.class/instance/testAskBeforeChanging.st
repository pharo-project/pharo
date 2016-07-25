tests
testAskBeforeChanging
	| state |
	self assert: buttonModel askBeforeChanging not.
	window := buttonModel openWithSpec.
	state := buttonModel widget askBeforeChanging.
	self assert: state not.
	buttonModel askBeforeChanging: true.
	state := buttonModel widget askBeforeChanging.
	self assert: state