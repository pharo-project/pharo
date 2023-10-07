Class {
	#name : 'InitializedSlotTest',
	#superclass : 'SlotSilentTest',
	#category : 'VariablesLibrary-Tests-Tests',
	#package : 'VariablesLibrary-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
InitializedSlotTest >> testBlockWithNonSharedDefault [
 	"If we use a block the resulting default value should not be shared among instances"

 	| slot objectA objectB defaultA defaultB |
 	slot := #slot1 => InitializedSlot default: [ OrderedCollection new ].
 	aClass := self make: [ :builder | builder slots: { slot } ].
 	self assert: (aClass hasSlotNamed: #slot1).

 	objectA := aClass new.
 	objectB := aClass new.

 	defaultA := slot read: objectA.
 	defaultB := slot read: objectB.

 	self assert: defaultA isEmpty.
 	self assert: defaultB isEmpty.

 	"Each instance has its own collection"
 	self deny: defaultA identicalTo: defaultB.

 	defaultA add: 'HelloWorld'.

 	self assert: (slot read: objectA) size equals: 1.
 	self assert: (slot read: objectB) size equals: 0
]

{ #category : 'tests' }
InitializedSlotTest >> testLazyClassVariableReflectiveBlockParameter [
	| classVar |
	classVar := #ClassVar => LazyClassVariable default: [:class | class].
	aClass := self make: [ :builder | builder sharedVariables: {classVar}].

	self assert: classVar read equals: aClass
]

{ #category : 'tests - printing' }
InitializedSlotTest >> testPrintOn [

 	| slot |
 	slot := #slot1 => InitializedSlot default: [ OrderedCollection new ].
	self
		assert: slot definitionString
		equals: '#slot1 => InitializedSlot default: [ OrderedCollection new ]'.

	slot := #slot1 => InitializedSlot default: #literal.
	self
		assert: slot definitionString
		equals: '#slot1 => InitializedSlot default: #literal'.

	"On eval, we raise an error, the ClassParser can show a nicer error in the class definition"
	self should: [#slot1 => InitializedSlot default: 5@2] raise: Error.
	self should: [#slot1 => InitializedSlot default: OrderedCollection new] raise: Error
]

{ #category : 'tests - read/write' }
InitializedSlotTest >> testReadWriteCompiled [
	| slot object|
	<ignoreNotImplementedSelectors: #(slot1 slot1:)>
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

{ #category : 'tests - read/write' }
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

{ #category : 'tests - read/write' }
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
