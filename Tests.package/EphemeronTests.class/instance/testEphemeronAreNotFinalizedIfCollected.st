tests
testEphemeronAreNotFinalizedIfCollected

	| finalized theKey ephemeron |
	Smalltalk supportsQueueingFinalization ifFalse: [ ^ self skip ].

	finalized := false.
	theKey := ObjectFinalizer receiver: [ finalized := true ] selector: #value.
	
	ephemeron := Ephemeron key: theKey value: nil.
	
	"Remove the only strong reference to the key and garbage collect to force mourning"
	theKey := ephemeron := nil.
	Smalltalk garbageCollect.
	
	self deny: finalized.