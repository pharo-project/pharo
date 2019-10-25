"
I am a slot allowing users to register behavior to execute when my value change. 

The classes using me should use TObservable alongside with me. This trait will give them methods to interact with me.

I am able to prevent infinit loop by locking the executions of the actions I store while they are executed. This is useful if two observable slots update each other during their registered actions.

Public API and Key Messages
--------------------

My users will not interact directly with me but via TObservable.
Check the class comment of this trait for more information.

Examples
--------------------

	Object subclass: #ObservablePoint
		uses: TObservable
		slots: { #x => ObservableSlot. #y }
		classVariables: {  }
		package: 'VariablesLibrary-Tests-Observable'
 
Internal Representation and Key Implementation Points.
--------------------

	I am wrapping a ObservableValueHolder in order to store the content of the variable and to register the actions to execute when it changes.
"
Class {
	#name : #ObservableSlot,
	#superclass : #IndexedSlot,
	#category : #'VariablesLibrary-Observable'
}

{ #category : #'code generation' }
ObservableSlot >> emitStore: aMethodBuilder [
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
ObservableSlot >> emitValue: aMethodBuilder [
	"Push the value holder into the stack"
	aMethodBuilder pushInstVar: index.
	aMethodBuilder send: #value
]

{ #category : #initialization }
ObservableSlot >> initialize: anObject [
	super write: ObservableValueHolder new to: anObject
]

{ #category : #testing }
ObservableSlot >> isObservable [
	^ true
]

{ #category : #'meta-object-protocol' }
ObservableSlot >> rawRead: anObject [
	^ super read: anObject
]

{ #category : #'meta-object-protocol' }
ObservableSlot >> read: anObject [
	^ (self rawRead: anObject) value
]

{ #category : #'meta-object-protocol' }
ObservableSlot >> wantsInitialization [
	^ true
]

{ #category : #'meta-object-protocol' }
ObservableSlot >> write: aValue to: anObject [
	(self rawRead: anObject) ifNotNil: [ :v | v value: aValue ].
	^ aValue
]
