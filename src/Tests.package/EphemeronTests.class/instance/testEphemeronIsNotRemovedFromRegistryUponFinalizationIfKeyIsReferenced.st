tests
testEphemeronIsNotRemovedFromRegistryUponFinalizationIfKeyIsReferenced

	| finalized theKey registry |
	Smalltalk supportsQueueingFinalization ifFalse: [ ^ self skip ].

	finalized := false.
	theKey := ObjectFinalizer receiver: [ finalized := true ] selector: #value.
	
	registry := EphemeronRegistry new.
	registry at: theKey put: nil.
		
	"Nil theKey to remove all strong references to it. Then Garbage collect to force mourning"
	Smalltalk garbageCollect.
	
	self assert: registry size equals: 1.