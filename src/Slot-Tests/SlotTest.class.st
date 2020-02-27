"
tests for Slots 
"
Class {
	#name : #SlotTest,
	#superclass : #TestCase,
	#instVars : [
		'ivarForTesting'
	],
	#category : #'Slot-Tests-VariablesAndSlots'
}

{ #category : #'tests - misc' }
SlotTest >> testAsSlotCollection [
	
 	self assert: 'a b' asSlotCollection equals: {#a => InstanceVariableSlot. #b => InstanceVariableSlot}.
	
	self assert: ('a _a a_a b b_' asSlotCollection collect: [ :i | i name ]) equals: #(#a #_a #a_a #b #b_) .
	
	self should: ['1' asSlotCollection] raise: Error.
	
	self should: ['ivarA ivarB i%varD iv2arF' asSlotCollection] raise: Error.
	self should: ['iva|rG' asSlotCollection] raise: Error





]

{ #category : #'tests - read/write' }
SlotTest >> testIsReadInMethod [
	
	ivarForTesting printString.
	self assert: ((self class slotNamed: #ivarForTesting) isReadIn: self class >> testSelector)
]

{ #category : #'tests - testing' }
SlotTest >> testIsReferenced [
	self assert: (Point slotNamed: #x) isReferenced
]

{ #category : #'tests - read/write' }
SlotTest >> testIsWrittenInMethod [
	
	ivarForTesting := #test.
	self assert: ((self class slotNamed: #ivarForTesting) isWrittenIn: self class >> testSelector)
]

{ #category : #'tests - misc' }
SlotTest >> testNotFoundSlotRaiseExplicitError [
	"When a slot is not present in the image, we will try to build the slot with nil. In that case we should return an error."

	self should: [ #test => nil ] raise: SlotNotFound
]

{ #category : #'tests - read/write' }
SlotTest >> testNotReadInMethod [
	
	self deny: ((self class slotNamed: #ivarForTesting) isReadIn: self class >> testSelector)
]

{ #category : #'tests - read/write' }
SlotTest >> testNotWrittenInMethodWhenItIsOnlyRead [
	
	ivarForTesting printString.
	self deny: ((self class slotNamed: #ivarForTesting) isWrittenIn: self class >> testSelector)
]

{ #category : #'tests - properties' }
SlotTest >> testPropertyAtPut [

	| testValue ivar |
	
	testValue := Date today.
	ivar := self class slotNamed: #ivarForTesting.
	
	ivar propertyAt: #testKeySelector put: testValue.
	self
		assert: (ivar propertyAt: #testKeySelector)
		equals: testValue.
	ivar removeProperty: #testKeySelector.
	self assert: ivar properties isNil
]

{ #category : #'tests - properties' }
SlotTest >> testRemoveProperty [
	| ivar | 
	ivar := self class slotNamed: #ivarForTesting.
	
	ivar propertyAt: #testKeySelector put: 1.
		
	ivar removeProperty: #testKeySelector.
	self
		assert: (ivar propertyAt: #testKeySelector)
		equals: nil.
		
	self assert: ivar properties isNil
]

{ #category : #'tests - misc' }
SlotTest >> testSlotUsers [
	self assert: (ToOneRelationSlot slotUsers includes: SlotExampleMovie).
	self deny: (ToOneRelationSlot slotUsers includes: SlotExamplePerson)
]

{ #category : #'tests - misc' }
SlotTest >> testisUsed [
	self assert: InstanceVariableSlot isUsed.
	self assert: IndexedSlot isUsed. "subclasses are users"
	self assert: ProcessLocalSlot isUsed. "references count as uses, too"
]
