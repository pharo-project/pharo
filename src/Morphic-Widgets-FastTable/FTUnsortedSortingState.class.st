"
Description
--------------------

I am a sorting state for unsorted table.
"
Class {
	#name : #FTUnsortedSortingState,
	#superclass : #FTAbstractSortingState,
	#category : #'Morphic-Widgets-FastTable-Sorting'
}

{ #category : #sorting }
FTUnsortedSortingState >> executeSortingOn: aDataSource [
	aDataSource unsort
]

{ #category : #accessing }
FTUnsortedSortingState >> nextState [
	^ FTAscendingSortingState context: self context
]

{ #category : #accessing }
FTUnsortedSortingState >> sortingIcon [
	^ self context morphicIconNamed: #arrowUpAndDown
]
