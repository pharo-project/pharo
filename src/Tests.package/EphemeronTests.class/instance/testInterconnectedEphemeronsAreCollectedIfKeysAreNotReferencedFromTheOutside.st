tests
testInterconnectedEphemeronsAreCollectedIfKeysAreNotReferencedFromTheOutside

	| finalized1 finalized2 theKey theValue ephemeron1 ephemeron2 |
	Smalltalk supportsQueueingFinalization ifFalse: [ ^ self skip ].

	finalized1 := finalized2 := false.
	theKey := ObjectFinalizer receiver: [ finalized1 := true ] selector: #value.
	theValue := ObjectFinalizer receiver: [ finalized2 := true ] selector: #value.
		
	ephemeron1 := Ephemeron key: theKey value: theValue.
	ephemeron2 := Ephemeron key: theValue value: theKey.
	
	"Remove the only strong reference to the key and garbage collect to force mourning"
	theKey := theValue := nil.
	2 timesRepeat: [ Smalltalk garbageCollect ].
	
	self assert: (finalized1 and: finalized2).