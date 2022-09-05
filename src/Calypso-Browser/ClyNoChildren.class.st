"
I am ClyNoChildren, an exception indicating that an operation on a parent in a parent-child structure failed due to the absence of child elements. 
"
Class {
	#name : #ClyNoChildren,
	#superclass : #Error,
	#instVars : [
		'parent'
	],
	#category : #'Calypso-Browser-Exceptions'
}

{ #category : #signalling }
ClyNoChildren class >> signalFor: aParentObject [
	"Create and signal a ClyNotFound exception for aParentObject in the default receiver."
			
	^ self new
		parent: aParentObject;
		signal
]

{ #category : #accessing }
ClyNoChildren >> parent [

	^ parent
]

{ #category : #accessing }
ClyNoChildren >> parent: anObject [

	parent := anObject
]
