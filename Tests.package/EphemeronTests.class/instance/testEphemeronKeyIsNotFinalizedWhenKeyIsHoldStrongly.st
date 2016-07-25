tests
testEphemeronKeyIsNotFinalizedWhenKeyIsHoldStrongly

	| finalized theKey ephemeron |
	Smalltalk supportsQueueingFinalization ifFalse: [ ^ self skip ].

	finalized := false.
	theKey := ObjectFinalizer receiver: [ finalized := true ] selector: #value.
	
	ephemeron := Ephemeron key: theKey value: nil.
	
	"Garbage collect to force mourning"
	Smalltalk garbageCollect.
	
	self deny: finalized.