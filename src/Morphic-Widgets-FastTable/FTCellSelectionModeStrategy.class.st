"
Description
--------------------

I am a strategy to manage the behaviour of a FTTableMorph in case we are in a cell selection mode.

My indexes are stored as tuples whose first element designate the selected row and second element the selected column.

Examples
--------------------

	FTCellSelectionModeStrategy table: aFTTableMorph
"
Class {
	#name : #FTCellSelectionModeStrategy,
	#superclass : #FTSelectionModeStrategy,
	#category : #'Morphic-Widgets-FastTable-Selection'
}

{ #category : #testing }
FTCellSelectionModeStrategy >> allowsCellSelection [
	^ true
]

{ #category : #accessing }
FTCellSelectionModeStrategy >> indexForRow: aRowIndex [
	^ {aRowIndex . self table selectedIndex second}
]

{ #category : #accessing }
FTCellSelectionModeStrategy >> indexFromPosition: aTuple [
	^ aTuple
]

{ #category : #accessing }
FTCellSelectionModeStrategy >> indexesToSelectInMultipleSelectionFrom: firstIndex to: endIndex [
	| stepV stepH |
	stepV := firstIndex first <= endIndex first ifTrue: [ 1 ] ifFalse: [ -1 ].
	stepH := firstIndex second <= endIndex second ifTrue: [ 1 ] ifFalse: [ -1 ].
	^ (firstIndex second to: endIndex second by: stepH) flatCollect: [ :v | (firstIndex first to: endIndex first by: stepV) collect: [ :h | {h . v} ] ]
]

{ #category : #testing }
FTCellSelectionModeStrategy >> is: anIndex aboveRow: aRowIndex [
	^ anIndex first < aRowIndex
]

{ #category : #testing }
FTCellSelectionModeStrategy >> is: anIndex afterColumn: aColumnIndex [
	^ anIndex second > aColumnIndex 
]

{ #category : #testing }
FTCellSelectionModeStrategy >> is: anIndex beforeColumn: aColumnIndex [
	^ anIndex second < aColumnIndex 
]

{ #category : #testing }
FTCellSelectionModeStrategy >> is: anIndex bellowRow: aRowIndex [
	^ anIndex first > aRowIndex
]

{ #category : #accessing }
FTCellSelectionModeStrategy >> nullIndex [
	^ #(0 0)
]

{ #category : #accessing }
FTCellSelectionModeStrategy >> rowIndexFrom: anIndex [
	^ anIndex first
]

{ #category : #selection }
FTCellSelectionModeStrategy >> selectAll [
	self table selectIndexes: ((1 to: self table numberOfRows) flatCollect: [ :rowIndex | (1 to: self table numberOfColumns) collect: [ :columnIndex | {rowIndex . columnIndex} ] ]) asArray
]

{ #category : #selection }
FTCellSelectionModeStrategy >> selectFirst [
	self table selectIndex: #(1 1)
]

{ #category : #selection }
FTCellSelectionModeStrategy >> selectLast [
	self table selectIndex: {self table numberOfRows . self table numberOfColumns}
]

{ #category : #accessing }
FTCellSelectionModeStrategy >> selectableIndexAbove: anIndex [
	^ {(anIndex first - 1) . anIndex second}
]

{ #category : #accessing }
FTCellSelectionModeStrategy >> selectableIndexAfter: anIndex [
	^ {anIndex first . (anIndex second + 1)}
]

{ #category : #accessing }
FTCellSelectionModeStrategy >> selectableIndexBefore: anIndex [
	^ {anIndex first . (anIndex second - 1)}
]

{ #category : #accessing }
FTCellSelectionModeStrategy >> selectableIndexBellow: anIndex [
	^ {(anIndex first + 1) . anIndex second}
]

{ #category : #accessing }
FTCellSelectionModeStrategy >> selectableIndexContainingPoint: aPoint [
	^ self tableContainer rowAndColumnIndexContainingPoint: aPoint
]

{ #category : #adding }
FTCellSelectionModeStrategy >> selectablesToHighlightFromRow: aFTRow at: aRowIndex withHighlightedIndexes: highligtedIndexes andPrimaryIndex: primarySelectionIndex [
	| selectablesToColorMap |
	selectablesToColorMap := Dictionary new.
	self flag: #pharoFixMe.	"Currently, the rows in FastTable have cells in submorphs, but the API should be improved to provide better cell iterations instead of letting external users touch row submorphs."
	aFTRow submorphs
		withIndexDo: [ :each :columnIndex | 
			| cellIndex |
			(highligtedIndexes includes: (cellIndex := {aRowIndex . columnIndex})) ifTrue: [ selectablesToColorMap at: each put: primarySelectionIndex = cellIndex ] ].
	^ selectablesToColorMap
]
