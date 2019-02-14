"
A FTDefaultColumnSortingStrategyTest is a test class for testing the behavior of FTDefaultColumnSortingStrategy
"
Class {
	#name : #FTDefaultColumnSortingStrategyTest,
	#superclass : #FTAbstractColumnSortingStrategyTest,
	#category : #'Morphic-Widgets-FastTable-Tests-Sorting'
}

{ #category : #accessing }
FTDefaultColumnSortingStrategyTest >> actualClass [
	^ FTDefaultColumnSortingStrategy
]

{ #category : #tests }
FTDefaultColumnSortingStrategyTest >> testProperty [
	| dataSource |
	dataSource := self dataSourceForTesting.
	strategy property: #yourself.
	self assert: dataSource elements equals: self elementsForTestDataSource.
	strategy sortDataSource: dataSource.
	self assert: dataSource elements equals: #(1 2 3 4 5 6 7 8 9 10).
	strategy reset.
	strategy property: #asString.
	strategy sortDataSource: dataSource.
	self assert: dataSource elements equals: #(1 10 2 3 4 5 6 7 8 9)
]

{ #category : #tests }
FTDefaultColumnSortingStrategyTest >> testReset [
	| dataSource |
	dataSource := self dataSourceForTesting.
	strategy property: #yourself.
	self assert: dataSource elements equals: self elementsForTestDataSource.
	strategy sortDataSource: dataSource.
	self assert: dataSource elements equals: #(1 2 3 4 5 6 7 8 9 10).
	strategy reset.
	strategy sortDataSource: dataSource.
	self assert: dataSource elements equals: #(1 2 3 4 5 6 7 8 9 10)
]

{ #category : #tests }
FTDefaultColumnSortingStrategyTest >> testSortDataSource [
	| dataSource |
	dataSource := self dataSourceForTesting.
	strategy property: #yourself.
	self assert: dataSource elements equals: self elementsForTestDataSource.
	strategy sortDataSource: dataSource.
	self assert: dataSource elements equals: #(1 2 3 4 5 6 7 8 9 10).
	strategy sortDataSource: dataSource.
	self assert: dataSource elements equals: #(10 9 8 7 6 5 4 3 2 1).
	strategy sortDataSource: dataSource.
	self assert: dataSource elements equals: self elementsForTestDataSource
]
