"
I sort objects in ascending order using the name of their defining class
"
Class {
	#name : #ClySortByDefiningClassFunction,
	#superclass : #ClySortFunction,
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #evaluating }
ClySortByDefiningClassFunction >> collate: anObject1 with: anObject2 [
	^anObject1 definingClass name threeWayCompareTo: anObject2 definingClass name
]
