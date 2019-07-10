"
A FTAbstractSortingStateTest is a test class for testing the behavior of FTAbstractSortingState
"
Class {
	#name : #FTAbstractSortingStateTest,
	#superclass : #TestCase,
	#instVars : [
		'state'
	],
	#category : #'Morphic-Widgets-FastTable-Tests-Sorting'
}

{ #category : #testing }
FTAbstractSortingStateTest class >> isAbstract [
	^ self = FTAbstractSortingStateTest
]

{ #category : #testing }
FTAbstractSortingStateTest class >> shouldInheritSelectors [
	^ true
]

{ #category : #accessing }
FTAbstractSortingStateTest >> actualClass [
	^ self subclassResponsibility
]

{ #category : #helpers }
FTAbstractSortingStateTest >> dataSourceForTesting [
	^ FTTableMorph new
		dataSource: (FTSimpleDataSource elements: self elementsForTestDataSource);
		dataSource
]

{ #category : #helpers }
FTAbstractSortingStateTest >> elementsForTestDataSource [
	^ #(2 4 7 1 5 8 6 9 10 3)
]

{ #category : #running }
FTAbstractSortingStateTest >> setUp [
	super setUp.
	state := self actualClass
		context:
			(FTPropertyColumnSortingStrategy new
				property: #yourself;
				yourself).
	state context state: state
]

{ #category : #tests }
FTAbstractSortingStateTest >> testExecuteSortingOn [
	self subclassResponsibility
]

{ #category : #tests }
FTAbstractSortingStateTest >> testSort [
	| dataSource |
	dataSource := self dataSourceForTesting.
	self assert: state context state equals: state.
	state sort: dataSource.
	self deny: state context state equals: state
]
