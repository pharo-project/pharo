"
I just sort given objects by name.
So I am supposed that sorted objects will respond to #name
"
Class {
	#name : #ClySortByNameFunction,
	#superclass : #ClySortFunction,
	#category : #'Calypso-NavigationModel-Model'
}

{ #category : #evaluating }
ClySortByNameFunction >> collate: anObject1 with: anObject2 [
	^anObject1 name threeWayCompareTo: anObject2 name
]
