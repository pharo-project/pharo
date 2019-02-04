Class {
	#name : #FTSelectionModeStrategy,
	#superclass : #Object,
	#instVars : [
		'table'
	],
	#category : #'Morphic-Widgets-FastTable-Selection'
}

{ #category : #'instance creation' }
FTSelectionModeStrategy class >> table: aTable [
	^ self new
		table: aTable;
		yourself
]

{ #category : #accessing }
FTSelectionModeStrategy >> indexFromPosition: aTuple [
	^ self subclassResponsibility
]

{ #category : #accessing }
FTSelectionModeStrategy >> indexesToSelectInMultipleSelectionFrom: firstIndex to: endIndex [
	"In case of multiple selection, I wust return the list of indexes to select when the user uses shift + click."

	^ self subclassResponsibility
]

{ #category : #testing }
FTSelectionModeStrategy >> is: anIndex aboveRow: aRowIndex [
	^ self subclassResponsibility
]

{ #category : #testing }
FTSelectionModeStrategy >> is: anIndex afterColumn: aColumnIndex [
	^ self subclassResponsibility
]

{ #category : #testing }
FTSelectionModeStrategy >> is: anIndex beforeColumn: aColumn [
	^ self subclassResponsibility
]

{ #category : #testing }
FTSelectionModeStrategy >> is: anIndex bellowRow: aRowIndex [
	^ self subclassResponsibility
]

{ #category : #testing }
FTSelectionModeStrategy >> isCellSelection [
	^ false
]

{ #category : #testing }
FTSelectionModeStrategy >> isRowSelection [
	^ false
]

{ #category : #accessing }
FTSelectionModeStrategy >> nullIndex [
	^ self subclassResponsibility
]

{ #category : #accessing }
FTSelectionModeStrategy >> rowIndexFrom: anIndex [
	^ self subclassResponsibility
]

{ #category : #selection }
FTSelectionModeStrategy >> selectAll [
	"This method should select all elements of the table."

	self subclassResponsibility
]

{ #category : #selection }
FTSelectionModeStrategy >> selectFirst [
	"This method should select the first element of the table."

	self subclassResponsibility
]

{ #category : #selection }
FTSelectionModeStrategy >> selectLast [
	"This method should select the first element of the table."

	self subclassResponsibility
]

{ #category : #accessing }
FTSelectionModeStrategy >> selectableIndexAbove: anIndex [
	^ self subclassResponsibility
]

{ #category : #accessing }
FTSelectionModeStrategy >> selectableIndexAfter: anIndex [
	^ self subclassResponsibility
]

{ #category : #accessing }
FTSelectionModeStrategy >> selectableIndexBefore: anIndex [
	^ self subclassResponsibility
]

{ #category : #accessing }
FTSelectionModeStrategy >> selectableIndexBellow: anIndex [
	^ self subclassResponsibility
]

{ #category : #accessing }
FTSelectionModeStrategy >> selectableIndexContainingPoint: aPoint [
	^ self subclassResponsibility
]

{ #category : #adding }
FTSelectionModeStrategy >> selectablesToHighlightFromRow: aFTRow at: aRowIndex withHighlightedIndexes: highligtedIndexes andPrimaryIndex: primarySelectionIndex [
	^ self subclassResponsibility
]

{ #category : #accessing }
FTSelectionModeStrategy >> selectedRowIndex [
	self flag: #todo.	"Check this is used at the end"
	^ self rowIndexFrom: self table selectedIndex
]

{ #category : #accessing }
FTSelectionModeStrategy >> table [
	^ table
]

{ #category : #accessing }
FTSelectionModeStrategy >> table: anObject [
	table := anObject
]

{ #category : #accessing }
FTSelectionModeStrategy >> tableContainer [
	^ self table container
]
