Class {
	#name : #SpecObservableSlot,
	#superclass : #IndexedSlot,
	#category : #'Spec-Core-Observable'
}

{ #category : #'code generation' }
SpecObservableSlot >> emitStore: aMethodBuilder [
	| temp |
	temp := Object new.
	"We pop the value from the stack into a temp to push it back in the right order"
	aMethodBuilder addTemp: temp.
	aMethodBuilder storeTemp: temp.
	
	"Push the value holder into the stack, then the value again, then send"
	aMethodBuilder pushInstVar: index.
	aMethodBuilder pushTemp: temp.
	aMethodBuilder send: #value:
]

{ #category : #'code generation' }
SpecObservableSlot >> emitValue: aMethodBuilder [
	"Push the value holder into the stack"
	aMethodBuilder pushInstVar: index.
	aMethodBuilder send: #value
]

{ #category : #initialization }
SpecObservableSlot >> initialize: anObject [

	super write: nil asValueHolder to: anObject
]

{ #category : #testing }
SpecObservableSlot >> isObservableSlot [
	
	^ true
]

{ #category : #initialization }
SpecObservableSlot >> rawRead: anObject [

	^ super read: anObject
]

{ #category : #initialization }
SpecObservableSlot >> read: anObject [

	^ (super read: anObject) value
]

{ #category : #initialization }
SpecObservableSlot >> wantsInitalization [

	^ true
]

{ #category : #initialization }
SpecObservableSlot >> write: aValue to: anObject [

	(super read: anObject) ifNotNil: [ :v | v value: aValue ].
	^ aValue
]
