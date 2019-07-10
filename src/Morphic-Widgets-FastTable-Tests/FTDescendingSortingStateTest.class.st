"
A FTDescendingSortingStateTest is a test class for testing the behavior of FTDescendingSortingState
"
Class {
	#name : #FTDescendingSortingStateTest,
	#superclass : #FTAbstractSortingStateTest,
	#category : #'Morphic-Widgets-FastTable-Tests-Sorting'
}

{ #category : #accessing }
FTDescendingSortingStateTest >> actualClass [
	^ FTDescendingSortingState
]

{ #category : #tests }
FTDescendingSortingStateTest >> testExecuteSortingOn [
	| dataSource |
	dataSource := self dataSourceForTesting.
	state executeSortingOn: dataSource.
	self assert: dataSource elements equals: #(10 9 8 7 6 5 4 3 2 1).
	state executeSortingOn: dataSource.
	self assert: dataSource elements equals: #(10 9 8 7 6 5 4 3 2 1).
]
