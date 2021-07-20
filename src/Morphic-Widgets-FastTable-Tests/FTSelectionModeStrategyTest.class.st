"
A FTSelectionModeStrategyTest is a test class for testing the behavior of FTSelectionModeStrategy
"
Class {
	#name : #FTSelectionModeStrategyTest,
	#superclass : #TestCase,
	#instVars : [
		'table',
		'strategy'
	],
	#category : #'Morphic-Widgets-FastTable-Tests-Selection'
}

{ #category : #testing }
FTSelectionModeStrategyTest class >> isAbstract [
	^ self = FTSelectionModeStrategyTest
]

{ #category : #testing }
FTSelectionModeStrategyTest class >> shouldInheritSelectors [
	^ true
]

{ #category : #running }
FTSelectionModeStrategyTest >> setStrategy [
	self subclassResponsibility
]

{ #category : #running }
FTSelectionModeStrategyTest >> setUp [
	super setUp.
	table := FTTableMorph new
		addColumn: (FTColumn id: 'column1');
		addColumn: (FTColumn id: 'column2');
		addColumn: (FTColumn id: 'column3');
		dataSource: (FTSimpleDataSource elements: (1 to: 10));
		yourself.
	self setStrategy.
	strategy := table selectionModeStrategy
]

{ #category : #tests }
FTSelectionModeStrategyTest >> testIndexFromPosition [
	self subclassResponsibility
]

{ #category : #tests }
FTSelectionModeStrategyTest >> testIndexesToSelectInMultipleSelectionFromTo [
	self subclassResponsibility
]

{ #category : #tests }
FTSelectionModeStrategyTest >> testIsAboveRow [
	self subclassResponsibility
]

{ #category : #tests }
FTSelectionModeStrategyTest >> testIsAfterColumn [
	self subclassResponsibility
]

{ #category : #tests }
FTSelectionModeStrategyTest >> testIsBeforeColumn [
	self subclassResponsibility
]

{ #category : #tests }
FTSelectionModeStrategyTest >> testIsBellowRow [
	self subclassResponsibility
]

{ #category : #tests }
FTSelectionModeStrategyTest >> testRowIndexFrom [
	self subclassResponsibility
]

{ #category : #tests }
FTSelectionModeStrategyTest >> testSelectAll [
	self subclassResponsibility
]

{ #category : #tests }
FTSelectionModeStrategyTest >> testSelectFirst [
	self subclassResponsibility
]

{ #category : #tests }
FTSelectionModeStrategyTest >> testSelectLast [
	self subclassResponsibility
]

{ #category : #tests }
FTSelectionModeStrategyTest >> testSelectableIndexAbove [
	self subclassResponsibility
]

{ #category : #tests }
FTSelectionModeStrategyTest >> testSelectableIndexAfter [
	self subclassResponsibility
]

{ #category : #tests }
FTSelectionModeStrategyTest >> testSelectableIndexBefore [
	self subclassResponsibility
]

{ #category : #tests }
FTSelectionModeStrategyTest >> testSelectableIndexBellow [
	self subclassResponsibility
]

{ #category : #tests }
FTSelectionModeStrategyTest >> testSelectedRowIndex [
	self subclassResponsibility
]
