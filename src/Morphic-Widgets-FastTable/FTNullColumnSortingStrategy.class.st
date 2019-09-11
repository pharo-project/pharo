"
Description
--------------------

I am class managing the behavior of the sorting of a FastTable when the sorting is disabled.

By default I will display nothing in the header cell and do nothing we I am asked to sort the datasource of the table.
"
Class {
	#name : #FTNullColumnSortingStrategy,
	#superclass : #FTAbstractColumnSortingStrategy,
	#category : #'Morphic-Widgets-FastTable-Sorting'
}

{ #category : #testing }
FTNullColumnSortingStrategy >> isSortingEnabled [
	^ false
]

{ #category : #initialization }
FTNullColumnSortingStrategy >> reset [
	"Nothing for this one"
]

{ #category : #sorting }
FTNullColumnSortingStrategy >> sortDataSource: aDataSource [
	"Nothing to do here"
]

{ #category : #accessing }
FTNullColumnSortingStrategy >> sortingIcon [
	^ nil
]
