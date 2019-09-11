"
A ProcessLocal is a slot that references different objects in different processes.

Internally it is implemented with a ProcessLocalVariable. Writing into the slot writes a field in the current process. 

myProcessLocalVariable value: anObject

Reading into the Slot reads the only field in the array.

myProcessLocalVariable value.

This slot inherits from IndexedSlot to guarantee that this slot has a real field inside an object.

This slot should be used wisely:

1) The value is stored weakly, an example of this would be:
obj := MyClass new.
obj local: Object new.
obj local. “anObject” <—— Now we see the object
Smalltalk garbageCollect.
obj local. “nil” <—— Now we don’t

2) This variable are stored in a WeakArray in the Process, so massive use of them will make the array in the processes start growing a lot and often.

"
Class {
	#name : #ProcessLocalSlot,
	#superclass : #IndexedSlot,
	#category : #'Slot-Examples-Base'
}

{ #category : #'code generation' }
ProcessLocalSlot >> emitStore: aMethodBuilder [
	"generate bytecode for 'varname value: <stackTop>'"
	| temp |
	temp := '0slotTempForStackManipulation'.
	"We pop the value from the stack into a temp to push it back in the right order"
	aMethodBuilder addTemp: temp.
	aMethodBuilder storeTemp: temp.
	aMethodBuilder popTop.
	
	"Push the process local variable into the stack, then the value again, then send"
	aMethodBuilder pushInstVar: index.
	aMethodBuilder pushTemp: temp.
	aMethodBuilder send: #value:
]

{ #category : #'code generation' }
ProcessLocalSlot >> emitValue: aMethodBuilder [
	"Push the process local variable into the stack"
	aMethodBuilder pushInstVar: index.
	aMethodBuilder send: #value
]

{ #category : #initialization }
ProcessLocalSlot >> initialize: anObject [
	super write: ProcessLocalVariable new to: anObject
]

{ #category : #'meta-object-protocol' }
ProcessLocalSlot >> read: anObject [

	^ (super read: anObject) value.
]

{ #category : #'meta-object-protocol' }
ProcessLocalSlot >> wantsInitialization [

	^ true
]

{ #category : #'meta-object-protocol' }
ProcessLocalSlot >> write: aValue to: anObject [

	(super read: anObject) value: aValue.
	^ aValue
]
