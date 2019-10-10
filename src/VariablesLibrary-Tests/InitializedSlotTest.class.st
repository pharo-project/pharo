Class {
	#name : #InitializedSlotTest,
	#superclass : #SlotSilentTest,
	#category : #'VariablesLibrary-Tests-Tests'
}

{ #category : #tests }
InitializedSlotTest >> testLazyClassVariableReflectiveBlockParameter [
	| classVar |
	classVar := #ClassVar => LazyClassVariable default: [:class | class].
	aClass := self make: [ :builder | builder sharedVariables: {classVar}].

	self assert: classVar read equals: aClass
]

{ #category : #tests }
InitializedSlotTest >> testReadWriteCompiled [
	| slot object|
	slot := #slot1 => InitializedSlot default: 5.
	aClass := self make: [ :builder | builder slots: {slot}].
	self assert: (aClass hasSlotNamed: #slot1).
	self compileAccessorsFor: slot.
	object := aClass new.

	"test compiled write and read"
	self assert: (object slot1) equals: 5.
	object slot1: 10.
	self assert: (object slot1) equals: 10
	
	
	
	
	
]

{ #category : #tests }
InitializedSlotTest >> testReflectiveReadWrite [
	| slot object|
	slot := #slot1 => InitializedSlot default: 5.
	aClass := self make: [ :builder | builder slots: {slot}].
	self assert: (aClass hasSlotNamed: #slot1).
	object := aClass new.

	"test reflective write and read"
	self assert: (slot read: object) equals: 5.
	slot write: 10 to: object.
	self assert: (slot read: object) equals: 10
	
	
	
	
	
]

{ #category : #tests }
InitializedSlotTest >> testReflectiveReadWriteBlock [
	| slot object|
	slot := #slot1 => InitializedSlot default: [4+1].
	aClass := self make: [ :builder | builder slots: {slot}].
	self assert: (aClass hasSlotNamed: #slot1).
	object := aClass new.

	"test reflective write and read"
	self assert: (slot read: object) equals: 5.
	slot write: 10 to: object.
	self assert: (slot read: object) equals: 10
	
	
	
	
	
]
