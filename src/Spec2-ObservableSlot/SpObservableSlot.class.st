Class {
	#name : #SpObservableSlot,
	#superclass : #IndexedSlot,
	#category : #'Spec2-ObservableSlot'
}

{ #category : #'code generation' }
SpObservableSlot >> emitStore: aMethodBuilder [
	"generate bytecode for 'varName value: <stackTop>'"

	| temp |
	temp := Object new.	"we need a unique Object as a temp name"
	"We pop the value from the stack into a temp to push it back in the right order"
	aMethodBuilder addTemp: temp.
	aMethodBuilder storeTemp: temp.
	aMethodBuilder popTop.

	"Push the value holder into the stack, then the value again, then send"
	aMethodBuilder pushInstVar: index.
	aMethodBuilder pushTemp: temp.
	aMethodBuilder send: #value:
]

{ #category : #'code generation' }
SpObservableSlot >> emitValue: aMethodBuilder [
	"Push the value holder into the stack"
	aMethodBuilder pushInstVar: index.
	aMethodBuilder send: #value
]

{ #category : #initialization }
SpObservableSlot >> initialize: anObject [
	super write: SpValueHolder new to: anObject
]

{ #category : #testing }
SpObservableSlot >> isObservableSlot [
	
	^ true
]

{ #category : #initialization }
SpObservableSlot >> rawRead: anObject [

	^ super read: anObject
]

{ #category : #initialization }
SpObservableSlot >> read: anObject [

	^ (super read: anObject) value
]

{ #category : #initialization }
SpObservableSlot >> wantsInitialization [

	^ true
]

{ #category : #initialization }
SpObservableSlot >> write: aValue to: anObject [

	(super read: anObject) ifNotNil: [ :v | v value: aValue ].
	^ aValue
]
