tests
testOneArgumentWithGC

	| m |
	m := WeakMessageSend
		receiver: Array
		selector: #with:
		arguments: { Object new }.
	Smalltalk garbageCollectMost.
	self assert: (m value isNil)