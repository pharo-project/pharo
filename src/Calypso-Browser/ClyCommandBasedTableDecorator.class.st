"
I decorate the main cell of the tables with the commands declared for the #tableIcons interaction of ClyCommandMap: each command which can run in the context of the item adds its clickable icon (ClyCommand>>#decorateTableCell:using:). See the guide doc/Calypso/Commands.md of the Pharo repository.
"
Class {
	#name : 'ClyCommandBasedTableDecorator',
	#superclass : 'ClyTableDecorator',
	#category : 'Calypso-Browser-Table',
	#package : 'Calypso-Browser',
	#tag : 'Table'
}

{ #category : 'decoration' }
ClyCommandBasedTableDecorator class >> decorateMainTableCell: anItemCellMorph inContext: itemContext [

	(ClyCommandRegistry default activatorsOf: #tableIcons inContext: itemContext) do: [ :commandActivator | commandActivator decorateTableCell: anItemCellMorph ]
]

{ #category : 'decoration' }
ClyCommandBasedTableDecorator class >> decorateTableCell: anItemCellMorph of: aDataSourceItem [
]

{ #category : 'decoration' }
ClyCommandBasedTableDecorator class >> priority [
	"command based decoration should be in most priority then anything else"
	^1000
]
