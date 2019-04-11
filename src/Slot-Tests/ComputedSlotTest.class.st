Class {
	#name : #ComputedSlotTest,
	#superclass : #SlotSilentTest,
	#category : #'Slot-Tests-Examples'
}

{ #category : #tests }
ComputedSlotTest >> testReadComputedSlotCompiled [
	| object |
	aClass := self
		make:
			[ :builder | builder slots: {(#slot1 => ComputedSlot with: [:obj | 3 + 2 ] )} ].
	self assert: (aClass hasSlotNamed: #slot1).
	
	object := aClass new.
	
	"The slot is virtual and does not use memory"
	self assert: object basicSize  equals: 0.
	
	"now compile an accessor and read"
	aClass compile: 'slot1 ^slot1' classified: 'accessing'.
	self assert: object slot1 equals: 5.
]

{ #category : #tests }
ComputedSlotTest >> testReadComputedSlotReflective [
	| slot object |
	aClass := self make: [ :builder | builder slots: {(#slot1 => ComputedSlot with: [:obj | 3 + 2 ] )} ].
	self assert: (aClass hasSlotNamed: #slot1).

	slot := aClass slotNamed: #slot1.
	object := aClass new.
	
	self assert: (slot read: object) equals: 5.
]

{ #category : #tests }
ComputedSlotTest >> testReadComputedSlotReflectiveNoArg [
	| slot object |
	aClass := self make: [ :builder | builder slots: {(#slot1 => ComputedSlot with: [ 3 + 2 ] )} ].
	self assert: (aClass hasSlotNamed: #slot1).

	slot := aClass slotNamed: #slot1.
	object := aClass new.
	
	self assert: (slot read: object) equals: 5.
]

{ #category : #tests }
ComputedSlotTest >> testWriteComputedSlotCompiled [
	| object |
	aClass := self
		make:
			[ :builder | builder slots: {(#slot1 => ComputedSlot with: [:obj | 3 + 2 ] )} ].
	self assert: (aClass hasSlotNamed: #slot1).
	
	object := aClass new.
	
	"now compile an accessor and write"
	aClass compile: 'slot1 ^slot1' classified: 'accessing'.
	aClass compile: 'slot1: anObject slot1 := anObject' classified: 'accessing'.
	object slot1: 10.
	"the write is ignored"
	self assert: object slot1 equals: 5.
]
