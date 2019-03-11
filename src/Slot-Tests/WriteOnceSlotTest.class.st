"
Automated tests for WriteOnceSlot
"
Class {
	#name : #WriteOnceSlotTest,
	#superclass : #TestCase,
	#category : #'Slot-Tests-Examples'
}

{ #category : #tests }
WriteOnceSlotTest >> testRead [
	| empty |
	empty := AClassUsingWriteOnceSlot new.
	self assert: empty read equals: nil
]

{ #category : #tests }
WriteOnceSlotTest >> testWriteAndRead [
	| empty |
	empty := AClassUsingWriteOnceSlot new.
	empty modify: 25.
	self assert: empty read equals: 25
]

{ #category : #tests }
WriteOnceSlotTest >> testWriteTwice [
	| empty |
	empty := AClassUsingWriteOnceSlot new.
	empty modify: 25.
	self should: [ empty modify: 27 ] raise: Error
]
