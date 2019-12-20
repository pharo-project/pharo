Class {
	#name : #LazySlotTest,
	#superclass : #SlotSilentTest,
	#category : #'VariablesLibrary-Tests-Tests'
}

{ #category : #tests }
LazySlotTest >> testLazySlotCompiled [
	| slot object|
	slot := #slot1 => LazySlot default: 5 .
	aClass := self make: [ :builder | builder slots: {slot}].

	object := aClass new.
	"now compile an accessor and read"
	self compileAccessorsFor: slot.
	self assert: object slot1 equals: 5
	

	
]

{ #category : #tests }
LazySlotTest >> testLazySlotPrint [
	| slot |
	slot := #mySlot => LazySlot default: 5 .
	self assert: slot printString equals: '#mySlot => LazySlot default: 5'.
	slot := #mySlot => LazySlot default: [5].
	self assert: slot printString equals: '#mySlot => LazySlot default: [ 5 ]'.
	"if the default is really a block"
	slot := #mySlot => LazySlot default: [5].
	self assert: slot printString equals: '#mySlot => LazySlot default: [ 5 ]' 
]

{ #category : #tests }
LazySlotTest >> testLazySlotReadReflective [
	| slot object|
	slot := #slot1 => LazySlot default: 5.
	aClass := self make: [ :builder | builder slots: {slot} ].
	
	"test reflective write and read"
	object := aClass new.

	self assert: (slot read: object) equals: 5.
	slot write: 10 to: object.
	self assert: (slot read: object) equals: 10
	
	
	
	
	
]

{ #category : #tests }
LazySlotTest >> testLazySlotReadReflectiveBlock [
	| slot object|
	slot := #slot1 => LazySlot default: [:o | o class] .
	aClass := self make: [ :builder | builder slots: {slot } ].
	
	"test reflective write and read"
	object := aClass new.

	self assert: (slot read: object) equals: aClass.
	slot write: 10 to: object.
	self assert: (slot read: object) equals: 10
	
	
	
	
	
]
