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
	| cellIndexes |
	self isMultipleSelection ifFalse: [ ^ self ].
	cellIndexes := OrderedCollection new.
	1 to: self table numberOfRows do: [ :rowIndex | 1 to: self table numberOfColumns do: [ :columnIndex | cellIndexes add: {rowIndex . columnIndex} ] ].
	self table selectIndexes: cellIndexes asArray
]

{ #category : #private }
FTCellSelectionStrategy >> selectFirst [
	self table selectCellIndexRow: 1 column: 1
]

{ #category : #private }
FTCellSelectionStrategy >> selectLast [
	self table selectCellIndexRow: self table numberOfRows column: self table numberOfColumns
]
