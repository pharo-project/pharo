"
I am special main table column which represent cells by ClyMainItemCellMorph.
My cells are responsible to show expansion icon and any other decoration tools: definition morph, extra tool morps.
I ask given queryView to decorate cell which will pass cell morph through all related table decorators.
"
Class {
	#name : #ClyMainTableColumn,
	#superclass : #ClyTableColumn,
	#category : #'Calypso-Browser-Table'
}

{ #category : #'instance creation' }
ClyMainTableColumn class >> default [
	^(self id: #item)
		displayItemName
]

{ #category : #'cells creation' }
ClyMainTableColumn >> createCellFor: aDataSourceItem in: aQueryView [

	| cell |
	cell := ClyMainItemCellMorph on: aDataSourceItem.
	displayBlock value: cell value: aDataSourceItem.
	aQueryView decorateMainTableCell: cell of: aDataSourceItem.
	cell build.
	^cell
]

{ #category : #testing }
ClyMainTableColumn >> isMainColumn [ 

	^ true
]
