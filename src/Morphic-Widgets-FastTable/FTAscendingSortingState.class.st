"
Description
--------------------

I am a sorting state for ascending sorted table.
"
Class {
	#name : #FTAscendingSortingState,
	#superclass : #FTAbstractSortingState,
	#category : #'Morphic-Widgets-FastTable-Sorting'
}

{ #category : #sorting }
FTAscendingSortingState >> executeSortingOn: aDataSource [
	aDataSource sortWith: [ :entity | self context property value: entity ] ascending
]

{ #category : #accessing }
FTAscendingSortingState >> nextState [
	^ FTDescendingSortingState context: self context
]

{ #category : #accessing }
FTAscendingSortingState >> sortingIcon [
	^ self context morphicIconNamed: #arrowUp
]
