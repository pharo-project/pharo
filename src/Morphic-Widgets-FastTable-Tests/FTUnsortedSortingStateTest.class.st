"
A FTUnsortedSortingStateTest is a test class for testing the behavior of FTUnsortedSortingState
"
Class {
	#name : #FTUnsortedSortingStateTest,
	#superclass : #FTAbstractSortingStateTest,
	#category : #'Morphic-Widgets-FastTable-Tests-Sorting'
}

{ #category : #accessing }
FTUnsortedSortingStateTest >> actualClass [
	^ FTUnsortedSortingState
]

{ #category : #tests }
FTUnsortedSortingStateTest >> testExecuteSortingOn [
	| dataSource |
	dataSource := self dataSourceForTesting.
	state executeSortingOn: dataSource.
	self assert: dataSource elements equals: self elementsForTestDataSource.
	state executeSortingOn: dataSource.
	self assert: dataSource elements equals: self elementsForTestDataSource
]
