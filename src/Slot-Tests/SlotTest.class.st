Class {
	#name : #SlotTest,
	#superclass : #TestCase,
	#instVars : [
		'ivarForTesting'
	],
	#category : #Slot-Tests
}

{ #category : #tests }
SlotTest >> testAsSlotCollection [
	
 	self assert: 'a b' asSlotCollection = {#a => InstanceVariableSlot. #b => InstanceVariableSlot}.
	
	self assert: ('a _a a_a b b_' asSlotCollection collect: [ :i | i name ]) = #(#a #_a #a_a #b #b_) .
	
	self assert: ('1' asSlotCollection collect: [ :i | i name ]) = #(#v1).
	
	self assert: ('ivarA ivarB
 
ivarC
i%varD iv2arF



iva|rG
ivarH ivarE
' asSlotCollection collect: #name) = #(#ivarA #ivarB #ivarC #ivarD #iv2arF #ivarG #ivarH #ivarE)
]

{ #category : #'tests - properties' }
SlotTest >> testIsReadInMethod [
	
	ivarForTesting printString.
	self assert: ((self class slotNamed: #ivarForTesting) isReadIn: self class >> testSelector)
]

{ #category : #'tests - properties' }
SlotTest >> testIsWrittenInMethod [
	
	ivarForTesting := #test.
	self assert: ((self class slotNamed: #ivarForTesting) isWrittenIn: self class >> testSelector)
]

{ #category : #'tests - properties' }
SlotTest >> testNotReadInMethod [
	
	self deny: ((self class slotNamed: #ivarForTesting) isReadIn: self class >> testSelector)
]

{ #category : #'tests - properties' }
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
	self assert: ivar properties isNil.
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
		
	self assert: ivar properties isNil.
]
