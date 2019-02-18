"
Description
--------------------

I am an abstract class defining the state management of FTAbstractColumnSortingStrategy subclasses using a state to sort the table.

My subclasses should only define a sort function to be used by the state to sort the FastTable.
 
Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	state:			<aSortingState> 	The state of the sorting (ascending, descending, unsorted).

"
Class {
	#name : #FTStateBasedColumnSortingStrategy,
	#superclass : #FTAbstractColumnSortingStrategy,
	#instVars : [
		'state'
	],
	#category : #'Morphic-Widgets-FastTable-Sorting'
}

{ #category : #accessing }
FTStateBasedColumnSortingStrategy >> ascendingSortFunction [
	^ self subclassResponsibility
]

{ #category : #initialization }
FTStateBasedColumnSortingStrategy >> initialize [
	super initialize.
	self reset
]

{ #category : #initialization }
FTStateBasedColumnSortingStrategy >> reset [
	self state: (FTUnsortedSortingState context: self)
]

{ #category : #sorting }
FTStateBasedColumnSortingStrategy >> sortDataSource: aDataSource [
	self state sort: aDataSource
]

{ #category : #accessing }
FTStateBasedColumnSortingStrategy >> sortingIcon [
	^ self state sortingIcon
]

{ #category : #accessing }
FTStateBasedColumnSortingStrategy >> state [
	^ state
]

{ #category : #accessing }
FTStateBasedColumnSortingStrategy >> state: anObject [
	state := anObject
]
