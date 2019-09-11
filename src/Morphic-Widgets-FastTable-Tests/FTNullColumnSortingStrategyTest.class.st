"
A FTNullColumnSortingStrategyTest is a test class for testing the behavior of FTNullColumnSortingStrategy
"
Class {
	#name : #FTNullColumnSortingStrategyTest,
	#superclass : #FTAbstractColumnSortingStrategyTest,
	#category : #'Morphic-Widgets-FastTable-Tests-Sorting'
}

{ #category : #accessing }
FTNullColumnSortingStrategyTest >> actualClass [
	^ FTNullColumnSortingStrategy
]

{ #category : #tests }
FTNullColumnSortingStrategyTest >> testSortDataSource [
	| dataSource |
	dataSource := self dataSourceForTesting.
	self assert: dataSource elements equals: self elementsForTestDataSource.
	strategy sortDataSource: dataSource.
	self assert: dataSource elements equals: self elementsForTestDataSource
]
