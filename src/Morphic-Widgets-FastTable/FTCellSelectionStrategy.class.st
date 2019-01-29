Class {
	#name : #FTCellSelectionStrategy,
	#superclass : #FTSelectionModeStrategy,
	#category : #'Morphic-Widgets-FastTable-Selection'
}

{ #category : #testing }
FTCellSelectionStrategy >> isCellSelection [
	^ true
]

{ #category : #private }
FTCellSelectionStrategy >> selectAll [
	self table selectAllCellIndexes
]

{ #category : #private }
FTCellSelectionStrategy >> selectFirst [
	self table selectCellIndexRow: 1 column: 1
]

{ #category : #private }
FTCellSelectionStrategy >> selectLast [
	self table selectCellIndexRow: self table numberOfRows column: self table numberOfColumns
]
