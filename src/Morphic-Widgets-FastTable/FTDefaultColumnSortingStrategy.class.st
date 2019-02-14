"
Description
--------------------

I am a strategy defining the default sorting behavior of a FastTable when the sorting is enabled. 

I'll have three step depending on the current sorting state: 
- Sort ascending
- Sort descending
- Unsort

To manage my state I uses a State design pattern. 	
 
Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	property:		<aValuable>			The property allowing to get the element on which applying the sorting.
	state:			<aSortingState> 	The state of the sorting (ascending, descending, unsorted).

"
Class {
	#name : #FTDefaultColumnSortingStrategy,
	#superclass : #FTAbstractColumnSortingStrategy,
	#instVars : [
		'state',
		'property'
	],
	#category : #'Morphic-Widgets-FastTable-Sorting'
}

{ #category : #'instance creation' }
FTDefaultColumnSortingStrategy class >> column: aColumn property: aValuable [
	^ (self column: aColumn)
		property: aValuable;
		yourself
]

{ #category : #initialization }
FTDefaultColumnSortingStrategy >> initialize [
	super initialize.
	self reset
]

{ #category : #accessing }
FTDefaultColumnSortingStrategy >> property [
	^ property
]

{ #category : #accessing }
FTDefaultColumnSortingStrategy >> property: aString [ 
	property := aString
]

{ #category : #initialization }
FTDefaultColumnSortingStrategy >> reset [
	self state: (FTUnsortedSortingState context: self)
]

{ #category : #sorting }
FTDefaultColumnSortingStrategy >> sortDataSource: aDataSource [
	self state sort: aDataSource
]

{ #category : #accessing }
FTDefaultColumnSortingStrategy >> sortingIcon [
	^ self state sortingIcon
]

{ #category : #accessing }
FTDefaultColumnSortingStrategy >> state [
	^ state
]

{ #category : #accessing }
FTDefaultColumnSortingStrategy >> state: anObject [
	state := anObject
]
