"
Description
--------------------


I manage the different selection modes of a FastTable.

I manage the different behaviours between row and cell selections. Maybe we could also add column selection later.

I am part of a Strategy design pattern and I am used by a FTTableMroph.

Examples
--------------------

	FTCellSelectionModeStrategy table: aFTTableMorph
 
Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	table:		<aFTTableMorph>		The fast table whose selection I manage.

"
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

{ #category : #testing }
FTSelectionModeStrategy >> allowsCellSelection [
	^ false
]

{ #category : #testing }
FTSelectionModeStrategy >> allowsRowSelection [
	^ false
]

{ #category : #accessing }
FTSelectionModeStrategy >> indexForRow: aRowIndex [
	^ self subclassResponsibility
]

{ #category : #accessing }
FTSelectionModeStrategy >> indexFromPosition: aTuple [
	"I receive a tuple representing a row and a column index in my table and I should return the index of the selectable element that can be selected at this position."

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

{ #category : #accessing }
FTSelectionModeStrategy >> nullIndex [
	^ self subclassResponsibility
]

{ #category : #accessing }
FTSelectionModeStrategy >> rowIndexFrom: anIndex [
	"From the index of the selectable I should return the row index."

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
	"When getting in this method we should already know there is a selectable above this one."

	^ self subclassResponsibility
]

{ #category : #accessing }
FTSelectionModeStrategy >> selectableIndexAfter: anIndex [
	"When getting in this method we should already know there is a selectable after this one."
	
	^ self subclassResponsibility
]

{ #category : #accessing }
FTSelectionModeStrategy >> selectableIndexBefore: anIndex [
	"When getting in this method we should already know there is a selectable before this one."

	^ self subclassResponsibility
]

{ #category : #accessing }
FTSelectionModeStrategy >> selectableIndexBellow: anIndex [
	"When getting in this method we should already know there is a selectable before this one."

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
