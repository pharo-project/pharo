"
I decorate table with bold font for method groups and methods when full browser shows the class side
"
Class {
	#name : 'ClyClassSideBoldTableDecorator',
	#superclass : 'ClyTableDecorator',
	#category : 'Calypso-SystemTools-FullBrowser-UI',
	#package : 'Calypso-SystemTools-FullBrowser',
	#tag : 'UI'
}

{ #category : 'decoration' }
ClyClassSideBoldTableDecorator class >> decorateTableCell: anItemCellMorph of: aDataSourceItem [

	anItemCellMorph label clyAddEmphasis: TextEmphasis bold
]

{ #category : 'testing' }
ClyClassSideBoldTableDecorator class >> wantsDecorateTableCellOf: aDataSourceItem [

	^aDataSourceItem browser metaLevelScope = ClyClassSideScope
]
