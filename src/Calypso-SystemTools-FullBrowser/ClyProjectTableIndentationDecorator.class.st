"
I decorate table of projects with full identation mode.
So projects tree in the full browser shifts children together with collapsing items.

I am always applied to the projects
"
Class {
	#name : #ClyProjectTableIndentationDecorator,
	#superclass : #ClyTableDecorator,
	#category : #'Calypso-SystemTools-FullBrowser-UI'
}

{ #category : #decoration }
ClyProjectTableIndentationDecorator class >> decorateMainTableCell: anItemCellMorph of: aDataSourceItem [
	super decorateMainTableCell: anItemCellMorph of: aDataSourceItem.
	
	anItemCellMorph useFullIndentation 
]

{ #category : #decoration }
ClyProjectTableIndentationDecorator class >> decorateTableCell: anItemCellMorph of: aDataSourceItem [

]

{ #category : #decoration }
ClyProjectTableIndentationDecorator class >> decorationStrategy [
	<classAnnotation>
	
	^ClyTableDecorationStrategy for: ClyFullBrowserContext
]

{ #category : #testing }
ClyProjectTableIndentationDecorator class >> wantsDecorateTableCellOf: aDataSourceItem [
	^aDataSourceItem rootParentItem isBasedOnItemType: ClyManagedProject 
]
