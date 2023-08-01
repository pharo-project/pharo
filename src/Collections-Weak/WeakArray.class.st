"
WeakArray is an array which holds only weakly on its elements. This means whenever an object is only referenced by instances of WeakArray it will be garbage collected.
"
Class {
	#name : #WeakArray,
	#superclass : #Array,
	#type : #weak,
	#classVars : [
		'FinalizationProcess',
		'FinalizationSemaphore'
	],
	#category : #'Collections-Weak-Base'
}

{ #category : #'class initialization' }
WeakArray class >> initialize [
	"Do we need to initialize specialObjectsArray?"

	Smalltalk specialObjectsArray size < 42 ifTrue: [ Smalltalk recreateSpecialObjectsArray ].

	FinalizationSemaphore := Smalltalk specialObjectsArray at: 42
]

{ #category : #private }
WeakArray class >> pvtCreateTemporaryObjectIn: tempObject [
	"We have to create the temporary object in a separate stack frame"
	tempObject at: 1 put: Object new
]

{ #category : #accessing }
WeakArray class >> runningFinalizationProcess [
	"Answer the FinalizationProcess I am running, if any"
	^FinalizationProcess
]
