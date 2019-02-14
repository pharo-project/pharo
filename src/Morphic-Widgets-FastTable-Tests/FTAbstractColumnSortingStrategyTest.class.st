"
A FTAbstractColumnSortingStrategyTest is a test class for testing the behavior of FTAbstractColumnSortingStrategy
"
Class {
	#name : #FTAbstractColumnSortingStrategyTest,
	#superclass : #TestCase,
	#instVars : [
		'strategy'
	],
	#category : #'Morphic-Widgets-FastTable-Tests-Sorting'
}

{ #category : #testing }
FTAbstractColumnSortingStrategyTest class >> isAbstract [
	^ self = FTAbstractColumnSortingStrategyTest
]

{ #category : #testing }
FTAbstractColumnSortingStrategyTest class >> shouldInheritSelectors [
	^ true
]

{ #category : #accessing }
FTAbstractColumnSortingStrategyTest >> actualClass [
	self subclassResponsibility
]

{ #category : #helpers }
FTAbstractColumnSortingStrategyTest >> dataSourceForTesting [
	^ FTTableMorph new
		dataSource: (FTSimpleDataSource elements: self elementsForTestDataSource);
		dataSource
]

{ #category : #helpers }
FTAbstractColumnSortingStrategyTest >> elementsForTestDataSource [
	^ #(2 4 7 1 5 8 6 9 10 3)
]

{ #category : #running }
FTAbstractColumnSortingStrategyTest >> setUp [
	super setUp.
	strategy := self actualClass column: FTColumn new
]

{ #category : #tests }
FTAbstractColumnSortingStrategyTest >> testSortDataSource [
	self subclassResponsibility
]
