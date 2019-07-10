"
A FTAscendingSortingStateTest is a test class for testing the behavior of FTAscendingSortingState
"
Class {
	#name : #FTAscendingSortingStateTest,
	#superclass : #FTAbstractSortingStateTest,
	#category : #'Morphic-Widgets-FastTable-Tests-Sorting'
}

{ #category : #accessing }
FTAscendingSortingStateTest >> actualClass [
	^ FTAscendingSortingState
]

{ #category : #tests }
FTAscendingSortingStateTest >> testExecuteSortingOn [
	| dataSource |
	dataSource := self dataSourceForTesting.
	state executeSortingOn: dataSource.
	self assert: dataSource elements equals: #(1 2 3 4 5 6 7 8 9 10).
	state executeSortingOn: dataSource.
	self assert: dataSource elements equals: #(1 2 3 4 5 6 7 8 9 10).
]
