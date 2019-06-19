"
I represent highlighted table items.

When data source is changed I responsible to update highlighted rows of table.
For example when user expands tree node highlighted indexes should be shifted when expansion happens before selection. Same should be done when items of data source are removed or added.

I just override how set up table selection by applying highlighted row indexes to table
"
Class {
	#name : #ClyHighlightingSelection,
	#superclass : #ClyDataSourceSelection,
	#category : #'Calypso-Browser-DataSource'
}

{ #category : #controlling }
ClyHighlightingSelection >> ensureVisibleLastItem [
	"highlighting should never force table visible position"
]

{ #category : #initialization }
ClyHighlightingSelection >> setUpSelectedRows: indexes in: aTableMorph silently: silentSelection [
	silentSelection
		ifTrue: [ aTableMorph basicHighlightIndexes: indexes ]
		ifFalse: [ "we should prevent ignoring new selection if indexes are still same"
			aTableMorph basicHighlightIndexes: #(-1000).
			aTableMorph highlightIndexes: indexes ]
]
