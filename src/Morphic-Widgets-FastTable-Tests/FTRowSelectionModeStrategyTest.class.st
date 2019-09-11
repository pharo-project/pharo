"
A FTRowSelectionModeStrategyTest is a test class for testing the behavior of FTRowSelectionModeStrategy
"
Class {
	#name : #FTRowSelectionModeStrategyTest,
	#superclass : #FTSelectionModeStrategyTest,
	#category : #'Morphic-Widgets-FastTable-Tests-Selection'
}

{ #category : #running }
FTRowSelectionModeStrategyTest >> setStrategy [
	table beRowSelection
]

{ #category : #test }
FTRowSelectionModeStrategyTest >> testIndexFromPosition [
	self assert: (strategy indexFromPosition: #(3 7)) equals: 3
]

{ #category : #test }
FTRowSelectionModeStrategyTest >> testIndexesToSelectInMultipleSelectionFromTo [
	self assert: (strategy indexesToSelectInMultipleSelectionFrom: 3 to: 11) asArray equals: #(3 4 5 6 7 8 9 10 11).
	self assert: (strategy indexesToSelectInMultipleSelectionFrom: 11 to: 3) asArray equals: #(11 10 9 8 7 6 5 4 3).
	self assert: (strategy indexesToSelectInMultipleSelectionFrom: 3 to: 3) asArray equals: #(3)
]

{ #category : #test }
FTRowSelectionModeStrategyTest >> testIsAboveRow [
	self assert: (strategy is: 3 aboveRow: 4).
	self deny: (strategy is: 3 aboveRow: 2).
	self deny: (strategy is: 3 aboveRow: 3)
]

{ #category : #test }
FTRowSelectionModeStrategyTest >> testIsAfterColumn [
	self deny: (strategy is: 3 afterColumn: 4).
	self deny: (strategy is: 3 afterColumn: 3).
	self deny: (strategy is: 3 afterColumn: 2)
]

{ #category : #test }
FTRowSelectionModeStrategyTest >> testIsBeforeColumn [
	self deny: (strategy is: 3 beforeColumn: 9).
	self deny: (strategy is: 3 beforeColumn: 4).
	self deny: (strategy is: 3 beforeColumn: 7)
]

{ #category : #test }
FTRowSelectionModeStrategyTest >> testIsBellowRow [
	self assert: (strategy is: 3 bellowRow: 2).
	self deny: (strategy is: 3 bellowRow: 4).
	self deny: (strategy is: 3 bellowRow: 3)
]

{ #category : #test }
FTRowSelectionModeStrategyTest >> testRowIndexFrom [
	self assert: (strategy rowIndexFrom: 3) equals: 3
]

{ #category : #test }
FTRowSelectionModeStrategyTest >> testSelectAll [
	table beSingleSelection.
	table selectFirst.
	table selectAll.
	self assert: table selectedIndexes equals: #(1).
	table beMultipleSelection.
	table selectFirst.
	table selectAll.
	self assert: table selectedIndexes equals: #(1 2 3 4 5 6 7 8 9 10)
]

{ #category : #test }
FTRowSelectionModeStrategyTest >> testSelectFirst [
	table selectFirst.
	self assert: table selectedIndexes equals: #(1)
]

{ #category : #test }
FTRowSelectionModeStrategyTest >> testSelectLast [
	table selectLast.
	self assert: table selectedIndexes equals: #(10)
]

{ #category : #test }
FTRowSelectionModeStrategyTest >> testSelectableIndexAbove [
	self assert: (strategy selectableIndexAbove: 2) equals: 1
]

{ #category : #test }
FTRowSelectionModeStrategyTest >> testSelectableIndexAfter [
	self should: [ strategy selectableIndexAfter: 2 ] raise: Error
]

{ #category : #test }
FTRowSelectionModeStrategyTest >> testSelectableIndexBefore [
	self should: [ strategy selectableIndexBefore: 2 ] raise: Error
]

{ #category : #test }
FTRowSelectionModeStrategyTest >> testSelectableIndexBellow [
	self assert: (strategy selectableIndexBellow: 2) equals: 3
]

{ #category : #test }
FTRowSelectionModeStrategyTest >> testSelectedRowIndex [
	table selectIndex: 3.
	self assert: strategy selectedRowIndex equals: 3.
	table selectIndex: 4.
	self assert: strategy selectedRowIndex equals: 4
]
