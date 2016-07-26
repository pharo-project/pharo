tests
testTwoArguments

	| m |
	m := WeakMessageSend
		receiver: Array
		selector: #with:with:
		arguments: { 1 . 2 }.
	Smalltalk garbageCollectMost.
	self assert: (m value = { 1 . 2 }).
