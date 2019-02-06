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

{ #category : #test }
FTSelectionModeStrategyTest >> testIndexFromPosition [
	self subclassResponsibility
]

{ #category : #test }
FTSelectionModeStrategyTest >> testIndexesToSelectInMultipleSelectionFromTo [
	self subclassResponsibility
]

{ #category : #test }
FTSelectionModeStrategyTest >> testIsAboveRow [
	self subclassResponsibility
]

{ #category : #test }
FTSelectionModeStrategyTest >> testIsAfterColumn [
	self subclassResponsibility
]

{ #category : #test }
FTSelectionModeStrategyTest >> testIsBeforeColumn [
	self subclassResponsibility
]

{ #category : #test }
FTSelectionModeStrategyTest >> testIsBellowRow [
	self subclassResponsibility
]

{ #category : #test }
FTSelectionModeStrategyTest >> testRowIndexFrom [
	self subclassResponsibility
]

{ #category : #test }
FTSelectionModeStrategyTest >> testSelectAll [
	self subclassResponsibility
]

{ #category : #test }
FTSelectionModeStrategyTest >> testSelectFirst [
	self subclassResponsibility
]

{ #category : #test }
FTSelectionModeStrategyTest >> testSelectLast [
	self subclassResponsibility
]

{ #category : #test }
FTSelectionModeStrategyTest >> testSelectableIndexAbove [
	self subclassResponsibility
]

{ #category : #test }
FTSelectionModeStrategyTest >> testSelectableIndexAfter [
	self subclassResponsibility
]

{ #category : #test }
FTSelectionModeStrategyTest >> testSelectableIndexBefore [
	self subclassResponsibility
]

{ #category : #test }
FTSelectionModeStrategyTest >> testSelectableIndexBellow [
	self subclassResponsibility
]

{ #category : #test }
FTSelectionModeStrategyTest >> testSelectedRowIndex [
	self subclassResponsibility
]
