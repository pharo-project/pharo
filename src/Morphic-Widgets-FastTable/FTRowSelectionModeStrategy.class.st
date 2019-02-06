"
Description
--------------------

I am a strategy to manage the behaviour of a FTTableMorph in case we are in a row selection mode.

My indexes are integers defining the index of the selected row.

Examples
--------------------

	FTRowSelectionModeStrategy table: aFTTableMorph
"
Class {
	#name : #FTRowSelectionModeStrategy,
	#superclass : #FTSelectionModeStrategy,
	#category : #'Morphic-Widgets-FastTable-Selection'
}

{ #category : #testing }
FTRowSelectionModeStrategy >> allowsRowSelection [
	^ true
]

{ #category : #accessing }
FTRowSelectionModeStrategy >> indexForRow: aRowIndex [
	^ aRowIndex
]

{ #category : #accessing }
FTRowSelectionModeStrategy >> indexFromPosition: aTuple [
	^ aTuple first
]

{ #category : #accessing }
FTRowSelectionModeStrategy >> indexesToSelectInMultipleSelectionFrom: firstIndex to: endIndex [
	| step |
	step := firstIndex <= endIndex ifTrue: [ 1 ] ifFalse: [ -1 ].
	^ firstIndex to: endIndex by: step
]

{ #category : #testing }
FTRowSelectionModeStrategy >> is: anIndex aboveRow: aRowIndex [
	^ anIndex < aRowIndex
]

{ #category : #testing }
FTRowSelectionModeStrategy >> is: anIndex afterColumn: aColumnIndex [
	"A row cannot be before or after a column."

	^ false
]

{ #category : #testing }
FTRowSelectionModeStrategy >> is: anIndex beforeColumn: aColumn [
	"A row cannot be before or after a column."

	^ false
]

{ #category : #testing }
FTRowSelectionModeStrategy >> is: anIndex bellowRow: aRowIndex [
	^ anIndex > aRowIndex
]

{ #category : #accessing }
FTRowSelectionModeStrategy >> nullIndex [
	^ 0
]

{ #category : #accessing }
FTRowSelectionModeStrategy >> rowIndexFrom: anIndex [
	^ anIndex
]

{ #category : #selection }
FTRowSelectionModeStrategy >> selectAll [
	self table selectIndexes: (1 to: self table numberOfRows) asArray
]

{ #category : #selection }
FTRowSelectionModeStrategy >> selectFirst [
	self table selectIndex: 1
]

{ #category : #selection }
FTRowSelectionModeStrategy >> selectLast [
	self table selectIndex: self table numberOfRows
]

{ #category : #accessing }
FTRowSelectionModeStrategy >> selectableIndexAbove: anIndex [
	^ anIndex - 1
]

{ #category : #accessing }
FTRowSelectionModeStrategy >> selectableIndexAfter: anIndex [
	"Because rows cannot have something at the right or left."

	self shouldNotImplement
]

{ #category : #accessing }
FTRowSelectionModeStrategy >> selectableIndexBefore: anIndex [
	"Because rows cannot have something at the right or left."

	self shouldNotImplement
]

{ #category : #accessing }
FTRowSelectionModeStrategy >> selectableIndexBellow: anIndex [
	^ anIndex + 1
]

{ #category : #accessing }
FTRowSelectionModeStrategy >> selectableIndexContainingPoint: aPoint [
	^ self tableContainer rowIndexContainingPoint: aPoint
]

{ #category : #adding }
FTRowSelectionModeStrategy >> selectablesToHighlightFromRow: aFTRow at: aRowIndex withHighlightedIndexes: highligtedIndexes andPrimaryIndex: primarySelectionIndex [
	| selectablesToColorMap |
	selectablesToColorMap := Dictionary new.
	(highligtedIndexes includes: aRowIndex) ifTrue: [ selectablesToColorMap at: aFTRow put: primarySelectionIndex = aRowIndex ].
	^ selectablesToColorMap
]
