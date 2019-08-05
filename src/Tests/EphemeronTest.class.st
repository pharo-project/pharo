Class {
	#name : #EphemeronTest,
	#superclass : #TestCase,
	#category : #'Tests-Finalization'
}

{ #category : #tests }
EphemeronTest >> testEphemeronAreNotFinalizedIfCollected [

	| finalized theKey ephemeron |
	Smalltalk supportsQueueingFinalization ifFalse: [ ^ self skip ].

	finalized := false.
	theKey := ObjectFinalizer receiver: [ finalized := true ] selector: #value.
	
	ephemeron := Ephemeron key: theKey value: nil.
	
	"Remove the only strong reference to the key and garbage collect to force mourning"
	theKey := ephemeron := nil.
	Smalltalk garbageCollect.
	
	self deny: finalized.
]

{ #category : #tests }
EphemeronTest >> testEphemeronIsNotRemovedFromRegistryUponFinalizationIfKeyIsReferenced [

	| finalized theKey registry |

	Smalltalk supportsQueueingFinalization ifFalse: [ ^ self skip ].

	finalized := false.
	theKey := ObjectFinalizer receiver: [ finalized := true ] selector: #value.

	registry := EphemeronRegistry new.
	registry at: theKey put: nil.	
	"Nil theKey to remove all strong references to it. Then Garbage collect to force mourning"
	Smalltalk garbageCollect.

	self
		assert: registry size equals: 1;
		assert: finalized
]

{ #category : #tests }
EphemeronTest >> testEphemeronIsRemovedFromRegistryUponFinalization [

	| finalized theKey registry |

	Smalltalk supportsQueueingFinalization ifFalse: [ ^ self skip ].

	finalized := false.
	theKey := ObjectFinalizer receiver: [ finalized := true ] selector: #value.

	registry := EphemeronRegistry new.
	registry at: theKey put: nil.	"Nil theKey to remove all strong references to it. Then Garbage collect to force mourning"
	theKey := nil.
	Smalltalk garbageCollect.

	self
		assertEmpty: registry;
		assert: finalized
]

{ #category : #tests }
EphemeronTest >> testEphemeronKeyIsFinalizedWhenKeyIsNotHoldStrongly [

	| finalized theKey ephemeron |
	Smalltalk supportsQueueingFinalization ifFalse: [ ^ self skip ].

	finalized := false.
	theKey := ObjectFinalizer receiver: [ finalized := true ] selector: #value.
	
	ephemeron := Ephemeron key: theKey value: nil.
	
	"Remove the only strong reference to the key and garbage collect to force mourning"
	theKey := nil.
	2 timesRepeat: [ Smalltalk garbageCollect ].
	
	self assert: finalized.
]

{ #category : #tests }
EphemeronTest >> testEphemeronKeyIsNotFinalizedWhenKeyIsHoldStrongly [

	| finalized theKey ephemeron |
	Smalltalk supportsQueueingFinalization ifFalse: [ ^ self skip ].

	finalized := false.
	theKey := ObjectFinalizer receiver: [ finalized := true ] selector: #value.
	
	ephemeron := Ephemeron key: theKey value: nil.
	
	"Garbage collect to force mourning"
	Smalltalk garbageCollect.
	
	self deny: finalized.
]

{ #category : #tests }
EphemeronTest >> testInterconnectedEphemeronsAreCollectedIfKeysAreNotReferencedFromTheOutside [

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
]

{ #category : #tests }
EphemeronTest >> testInterconnectedEphemeronsAreNotCollectedIfAKeyIsReferencedFromTheOutside [

	| finalized1 finalized2 theKey theValue ephemeron1 ephemeron2 |
	Smalltalk supportsQueueingFinalization ifFalse: [ ^ self skip ].

	finalized1 := finalized2 := false.
	theKey := ObjectFinalizer receiver: [ finalized1 := true ] selector: #value.
	theValue := ObjectFinalizer receiver: [ finalized2 := true ] selector: #value.
		
	ephemeron1 := Ephemeron key: theKey value: theValue.
	ephemeron2 := Ephemeron key: theValue value: theKey.
	
	"Remove only the strong reference to the first key and garbage collect to force mourning"
	theValue := nil.
	Smalltalk garbageCollect.
	
	self deny: finalized1.
	self deny: finalized2.
]

{ #category : #tests }
EphemeronTest >> testInterconnectedEphemeronsAreNotCollectedIfTheOtherKeyIsReferencedFromTheOutside [

	| finalized1 finalized2 theKey theValue ephemeron1 ephemeron2 |
	Smalltalk supportsQueueingFinalization ifFalse: [ ^ self skip ].

	finalized1 := finalized2 := false.
	theKey := ObjectFinalizer receiver: [ finalized1 := true ] selector: #value.
	theValue := ObjectFinalizer receiver: [ finalized2 := true ] selector: #value.
		
	ephemeron1 := Ephemeron key: theKey value: theValue.
	ephemeron2 := Ephemeron key: theValue value: theKey.
	
	"Remove only the strong reference to the second key and garbage collect to force mourning"
	theKey := nil.
	Smalltalk garbageCollect.
	
	self deny: finalized1.
	self deny: finalized2.
]
