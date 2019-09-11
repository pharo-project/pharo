"
A wrapper used by the PointerExplorer.
I wrap a reference so we can stop follwoing back pointers.
"
Class {
	#name : #EyePointerWrapper,
	#superclass : #Object,
	#instVars : [
		'wrapped'
	],
	#category : #'Spec2-Inspector-Element'
}

{ #category : #'instance creation' }
EyePointerWrapper class >> wrap: anObject [
	^self new wrappedObject: anObject.
]

{ #category : #printing }
EyePointerWrapper >> printString [
	^wrapped printString
]

{ #category : #accessing }
EyePointerWrapper >> wrappedObject [
	^wrapped
]

{ #category : #accessing }
EyePointerWrapper >> wrappedObject: anObject [
	wrapped := anObject
]
