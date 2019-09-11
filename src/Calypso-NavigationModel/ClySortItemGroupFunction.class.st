"
I implement sorting groups, kind of ClyItemGroup.

I sort them first by priority and after by name
"
Class {
	#name : #ClySortItemGroupFunction,
	#superclass : #ClySortFunction,
	#category : #'Calypso-NavigationModel-Model'
}

{ #category : #evaluating }
ClySortItemGroupFunction >> collate: aGroup1 with: aGroup2 [
	"ascending order by name but descending by priority"
	
	aGroup1 priority = aGroup2 priority ifTrue: [ 
		^aGroup1 name threeWayCompareTo: aGroup2 name].
	
	^aGroup2 priority threeWayCompareTo: aGroup1 priority
]
