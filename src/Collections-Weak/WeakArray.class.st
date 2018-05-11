"
WeakArray is an array which holds only weakly on its elements. This means whenever an object is only referenced by instances of WeakArray it will be garbage collected.
"
Class {
	#name : #WeakArray,
	#superclass : #Array,
	#type : #weak,
	#classVars : [
		'FinalizationDependents',
		'FinalizationLock',
		'FinalizationProcess',
		'FinalizationSemaphore'
	],
	#category : #'Collections-Weak-Base'
}

{ #category : #accessing }
WeakArray class >> addWeakDependent: anObject [
	
	FinalizationLock critical:[ | index finished weakDependent |
		finished := false.
		index := 0.
		[index := index + 1.
		finished not and:[index <= FinalizationDependents size]] whileTrue:[
			weakDependent := FinalizationDependents at: index.
			weakDependent ifNil: [
				FinalizationDependents at: index put: anObject.
				finished := true.
			].
		].
		finished ifFalse:[
			"Grow linearly"
			FinalizationDependents := FinalizationDependents, (WeakArray new: 10).
			FinalizationDependents at: index put: anObject.
		].
	] ifError:[:msg :rcvr| rcvr error: msg].
]

{ #category : #initialization }
WeakArray class >> initialize [
	"Do we need to initialize specialObjectsArray?"
	Smalltalk specialObjectsArray size < 42 
		ifTrue: [Smalltalk recreateSpecialObjectsArray].

	SessionManager default
		registerSystemClassNamed: self name.

	FinalizationSemaphore := Smalltalk specialObjectsArray at: 42.
	FinalizationLock := Semaphore forMutualExclusion.
	FinalizationDependents ifNil: [FinalizationDependents := WeakArray new: 10].
]

{ #category : #private }
WeakArray class >> pvtCreateTemporaryObjectIn: tempObject [
	"We have to create the temporary object in a separate stack frame"
	tempObject at: 1 put: Object new
]

{ #category : #accessing }
WeakArray class >> removeWeakDependent: anObject [
	FinalizationLock critical:[
		1 to: FinalizationDependents size do:[:i|
			((FinalizationDependents at: i) == anObject) ifTrue:[
				FinalizationDependents at: i put: nil.
			].
		].
	] ifError:[:msg :rcvr| rcvr error: msg].
]

{ #category : #accessing }
WeakArray class >> runningFinalizationProcess [
	"Answer the FinalizationProcess I am running, if any"
	^FinalizationProcess
]
