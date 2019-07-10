"
I implement sorting of ClyBrowserItem instances by their actual objects.
And to sort actual objects I include another sort function in the variable actualObjectSortFunction.

To create my instances use following script:

	ClySortBrowserItemFunction with: actualObjectSortFunction
	
Or simply convert any sort function using: 

	aSortFuntion forBrowserItems	
		 
Internal Representation and Key Implementation Points.

    Instance Variables
	actualObjectSortFunction:		<SortFunction>
"
Class {
	#name : #ClySortBrowserItemFunction,
	#superclass : #ClySortFunction,
	#instVars : [
		'actualObjectSortFunction'
	],
	#category : #'Calypso-NavigationModel-Model'
}

{ #category : #'instance creation' }
ClySortBrowserItemFunction class >> with: actualObjectSortFunction [
	^self new 
		actualObjectSortFunction: actualObjectSortFunction
]

{ #category : #comparing }
ClySortBrowserItemFunction >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	super = anObject ifFalse: [ ^ false ].
	^ actualObjectSortFunction = anObject actualObjectSortFunction
]

{ #category : #accessing }
ClySortBrowserItemFunction >> actualObjectSortFunction [
	^ actualObjectSortFunction
]

{ #category : #accessing }
ClySortBrowserItemFunction >> actualObjectSortFunction: anObject [
	actualObjectSortFunction := anObject
]

{ #category : #evaluating }
ClySortBrowserItemFunction >> collate: aBrowserItem1 with: aBrowserItem2 [
	^actualObjectSortFunction 
		collate: aBrowserItem1 actualObject 
		with: aBrowserItem2 actualObject
]

{ #category : #comparing }
ClySortBrowserItemFunction >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^super hash bitXor: actualObjectSortFunction hash
]
