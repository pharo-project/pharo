"
I delegate decoration of table to all commands annotated by ClyTableIconCommandActivation.
I allow extend table cell with command based iconic buttons. 
Any command class which is annotated with ClyTableIconCommandActivation will be represented by clickable icon in main table cell
"
Class {
	#name : #ClyCommandBasedTableDecorator,
	#superclass : #ClyTableDecorator,
	#category : #'Calypso-Browser-Table'
}

{ #category : #decoration }
ClyCommandBasedTableDecorator class >> decorateMainTableCell: anItemCellMorph inContext: itemContext [

	ClyTableIconCommandActivation 
		activateAllInContext: itemContext 
		by: [:commandActivator | 
			commandActivator decorateTableCell: anItemCellMorph ]
]

{ #category : #decoration }
ClyCommandBasedTableDecorator class >> decorateTableCell: anItemCellMorph of: aDataSourceItem [
]

{ #category : #decoration }
ClyCommandBasedTableDecorator class >> decorationStrategy [
	<classAnnotation>
	
	^ClyTableDecorationStrategy for: ClyBrowserContext
]

{ #category : #decoration }
ClyCommandBasedTableDecorator class >> priority [
	"command based decoration should be in most priority then anything else"
	^1000
]
