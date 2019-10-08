"
A WeakSlot is a slot that references weakly an object.

Internally it is implemented with a weak array of size one. Writing into the slot writes the only field in the array. 

weakArray at: 1 put: aValue.

Reading into the Slot reads the only field in the array.

weakArray at: 1 .

This slot inherits from IndexedSlot to guarantee that this slot has a real field inside an object.
"
Class {
	#name : #WeakSlot,
	#superclass : #IndexedSlot,
	#category : #'VariablesLibrary-Slots'
}

{ #category : #'code generation' }
WeakSlot >> emitStore: aMethodBuilder [
	"generate bytecode for 'varname at: 1 put: <stackTop>'"
	| temp |
	temp := '0slotTempForStackManipulation'.
	"Pop the value to store into a temp to push it back in the right order"
	aMethodBuilder addTemp: temp.
	aMethodBuilder storeTemp: temp.
	aMethodBuilder popTop.
	
	"Push the weak array into the stack, then the arguments, then send"
	aMethodBuilder pushInstVar: index.
	aMethodBuilder pushLiteral: 1.
	aMethodBuilder pushTemp: temp.
	aMethodBuilder send: #at:put:
]

{ #category : #'code generation' }
WeakSlot >> emitValue: aMethodBuilder [
	"Push the weak array at: 1 on the stack"
	aMethodBuilder pushInstVar: index.
	aMethodBuilder pushLiteral: 1.
	aMethodBuilder send: #at:
]

{ #category : #initialization }
WeakSlot >> initialize: anObject [

	super write: (WeakArray new: 1) to: anObject
]

{ #category : #'meta-object-protocol' }
WeakSlot >> read: anObject [

	^ (super read: anObject) at: 1.
]

{ #category : #'meta-object-protocol' }
WeakSlot >> wantsInitialization [

	^ true
]

{ #category : #'meta-object-protocol' }
WeakSlot >> write: aValue to: anObject [

	^(super read: anObject) at: 1 put: aValue
]
