"
A FTCellSelectionModeStrategyTest is a test class for testing the behavior of FTCellSelectionModeStrategy
"
Class {
	#name : #FTCellSelectionModeStrategyTest,
	#superclass : #FTSelectionModeStrategyTest,
	#category : #'Morphic-Widgets-FastTable-Tests-Selection'
}

{ #category : #running }
FTCellSelectionModeStrategyTest >> setStrategy [
	table beCellSelection
]

{ #category : #test }
FTCellSelectionModeStrategyTest >> testIndexFromPosition [
	self assert: (strategy indexFromPosition: #(3 7)) equals: #(3 7)
]

{ #category : #test }
FTCellSelectionModeStrategyTest >> testIndexesToSelectInMultipleSelectionFromTo [
	self
		assert: (strategy indexesToSelectInMultipleSelectionFrom: #(8 1) to: #(21 3)) asArray
		equals:
			#(#(8 1) #(9 1) #(10 1) #(11 1) #(12 1) #(13 1) #(14 1) #(15 1) #(16 1) #(17 1) #(18 1) #(19 1) #(20 1) #(21 1) #(8 2) #(9 2) #(10 2) #(11 2) #(12 2) #(13 2) #(14 2) #(15 2) #(16 2) #(17 2) #(18 2) #(19 2) #(20 2) #(21 2) #(8 3) #(9 3) #(10 3) #(11 3) #(12 3) #(13 3) #(14 3) #(15 3) #(16 3) #(17 3) #(18 3) #(19 3) #(20 3) #(21 3))
]

{ #category : #test }
FTCellSelectionModeStrategyTest >> testIsAboveRow [
	self assert: (strategy is: #(3 7) aboveRow: 4).
	self deny: (strategy is: #(3 7) aboveRow: 2).
	self deny: (strategy is: #(3 7) aboveRow: 3)
]

{ #category : #test }
FTCellSelectionModeStrategyTest >> testIsAfterColumn [
	self assert: (strategy is: #(3 7) afterColumn: 4).
	self deny: (strategy is: #(3 7) afterColumn: 9).
	self deny: (strategy is: #(3 7) afterColumn: 7)
]

{ #category : #test }
FTCellSelectionModeStrategyTest >> testIsBeforeColumn [
	self assert: (strategy is: #(3 7) beforeColumn: 9).
	self deny: (strategy is: #(3 7) beforeColumn: 4).
	self deny: (strategy is: #(3 7) beforeColumn: 7)
]

{ #category : #test }
FTCellSelectionModeStrategyTest >> testIsBellowRow [
	self assert: (strategy is: #(3 7) bellowRow: 2).
	self deny: (strategy is: #(3 7) bellowRow: 4).
	self deny: (strategy is: #(3 7) bellowRow: 3)
]

{ #category : #test }
FTCellSelectionModeStrategyTest >> testRowIndexFrom [
	self assert: (strategy rowIndexFrom: #(3 5)) equals: 3
]

{ #category : #test }
FTCellSelectionModeStrategyTest >> testSelectAll [
	table beSingleSelection.
	table selectFirst.
	table selectAll.
	self assert: table selectedIndexes equals: #(#(1 1)).
	table beMultipleSelection.
	table selectFirst.
	table selectAll.
	self
		assert: table selectedIndexes
		equals:
			#(#(1 1) #(1 2) #(1 3) #(2 1) #(2 2) #(2 3) #(3 1) #(3 2) #(3 3) #(4 1) #(4 2) #(4 3) #(5 1) #(5 2) #(5 3) #(6 1) #(6 2) #(6 3) #(7 1) #(7 2) #(7 3) #(8 1) #(8 2) #(8 3) #(9 1) #(9 2) #(9 3) #(10 1) #(10 2) #(10 3))
]

{ #category : #test }
FTCellSelectionModeStrategyTest >> testSelectFirst [
	table selectFirst.
	self assert: table selectedIndexes equals: #(#(1 1))
]

{ #category : #test }
FTCellSelectionModeStrategyTest >> testSelectLast [
	table selectLast.
	self assert: table selectedIndexes equals: #(#(10 3))
]

{ #category : #test }
FTCellSelectionModeStrategyTest >> testSelectableIndexAbove [
	self assert: (strategy selectableIndexAbove: #(2 3)) equals: #(1 3)
]

{ #category : #test }
FTCellSelectionModeStrategyTest >> testSelectableIndexAfter [
	self assert: (strategy selectableIndexAfter: #(2 3)) equals: #(2 4)
]

{ #category : #test }
FTCellSelectionModeStrategyTest >> testSelectableIndexBefore [
	self assert: (strategy selectableIndexBefore: #(2 3)) equals: #(2 2)
]

{ #category : #test }
FTCellSelectionModeStrategyTest >> testSelectableIndexBellow [
	self assert: (strategy selectableIndexBellow: #(2 3)) equals: #(3 3)
]

{ #category : #test }
FTCellSelectionModeStrategyTest >> testSelectedRowIndex [
	table selectIndex: #(3 5).
	self assert: strategy selectedRowIndex equals: 3.
	table selectIndex: #(4 5).
	self assert: strategy selectedRowIndex equals: 4
]
