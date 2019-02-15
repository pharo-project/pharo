"
A FTSortFunctionColumnSortingStrategyTest is a test class for testing the behavior of FTSortFunctionColumnSortingStrategy
"
Class {
	#name : #FTSortFunctionColumnSortingStrategyTest,
	#superclass : #FTAbstractColumnSortingStrategyTest,
	#category : #'Morphic-Widgets-FastTable-Tests-Sorting'
}

{ #category : #accessing }
FTSortFunctionColumnSortingStrategyTest >> actualClass [
	^ FTSortFunctionColumnSortingStrategy
]

{ #category : #helpers }
FTSortFunctionColumnSortingStrategyTest >> complexDataSourceForTesting [
	^ FTTableMorph new
		dataSource: (FTSimpleDataSource elements: self complexElementsForTestDataSource);
		dataSource
]

{ #category : #helpers }
FTSortFunctionColumnSortingStrategyTest >> complexElementsForTestDataSource [
	^ #(3 80 29 10 20 28 70)
]

{ #category : #tests }
FTSortFunctionColumnSortingStrategyTest >> testReset [
	| dataSource |
	dataSource := self dataSourceForTesting.
	strategy sortFunction: #yourself ascending.
	self assert: dataSource elements equals: self elementsForTestDataSource.
	strategy sortDataSource: dataSource.
	self assert: dataSource elements equals: #(1 2 3 4 5 6 7 8 9 10).
	strategy reset.
	strategy sortDataSource: dataSource.
	self assert: dataSource elements equals: #(1 2 3 4 5 6 7 8 9 10)
]

{ #category : #tests }
FTSortFunctionColumnSortingStrategyTest >> testSortDataSource [
	| dataSource |
	dataSource := self dataSourceForTesting.
	strategy sortFunction: #yourself ascending.
	self assert: dataSource elements equals: self elementsForTestDataSource.
	strategy sortDataSource: dataSource.
	self assert: dataSource elements equals: #(1 2 3 4 5 6 7 8 9 10).
	strategy sortDataSource: dataSource.
	self assert: dataSource elements equals: #(10 9 8 7 6 5 4 3 2 1).
	strategy sortDataSource: dataSource.
	self assert: dataSource elements equals: self elementsForTestDataSource
]

{ #category : #tests }
FTSortFunctionColumnSortingStrategyTest >> testSortDataSourceWithChainedSortFunction [
	| dataSource |
	dataSource := self complexDataSourceForTesting.
	strategy sortFunction: [ :element | element \\ 10 ] ascending , #yourself ascending.
	self assert: dataSource elements equals: self complexElementsForTestDataSource.
	strategy sortDataSource: dataSource.
	self assert: dataSource elements equals: #(10 20 70 80 3  28 29).
	strategy sortDataSource: dataSource.
	self assert: dataSource elements equals: #(29 28 3 80 70 20 10).
	strategy sortDataSource: dataSource.
	self assert: dataSource elements equals: self complexElementsForTestDataSource
]

{ #category : #tests }
FTSortFunctionColumnSortingStrategyTest >> testSortFunction [
	| dataSource |
	dataSource := self dataSourceForTesting.
	strategy sortFunction: #yourself ascending.
	self assert: dataSource elements equals: self elementsForTestDataSource.
	strategy sortDataSource: dataSource.
	self assert: dataSource elements equals: #(1 2 3 4 5 6 7 8 9 10).
	strategy reset.
	strategy sortFunction: #asString ascending.
	strategy sortDataSource: dataSource.
	self assert: dataSource elements equals: #(1 10 2 3 4 5 6 7 8 9)
]
