"
A TestingAbstractTwoButtons is a stupid AbstractTwoButtons used to test it.
"
Class {
	#name : #TestingAbstractTwoButtons,
	#superclass : #AbstractTwoButtons,
	#category : #'Spec-Tests-Utils'
}

{ #category : #examples }
TestingAbstractTwoButtons class >> example [
	<sampleInstance>
	^ self abstractExample openWithSpec 
]

{ #category : #initialization }
TestingAbstractTwoButtons >> initializeButtons [
]
