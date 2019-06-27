"
A wrapper used by the PointerExplorer.
I wrap a reference so we can stop follwoing back pointers.
"
Class {
	#name : #SpEyePointerWrapper,
	#superclass : #Object,
	#instVars : [
		'wrapped'
	],
	#category : #'Spec2-Inspector-Element'
}

{ #category : #'instance creation' }
SpEyePointerWrapper class >> wrap: anObject [
	^self new wrappedObject: anObject.
]

{ #category : #printing }
SpEyePointerWrapper >> printString [
	^wrapped printString
]

{ #category : #accessing }
SpEyePointerWrapper >> wrappedObject [
	^wrapped
]

{ #category : #accessing }
SpEyePointerWrapper >> wrappedObject: anObject [
	wrapped := anObject
]
