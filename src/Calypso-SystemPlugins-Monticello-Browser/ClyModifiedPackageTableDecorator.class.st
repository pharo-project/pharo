"
I decorate dirty packages in the table using special icon.
I am active only when ClyMonticelloBrowserPlugin is installed into the browser
"
Class {
	#name : #ClyModifiedPackageTableDecorator,
	#superclass : #ClyPackageTableDecorator,
	#category : #'Calypso-SystemPlugins-Monticello-Browser'
}

{ #category : #decoration }
ClyModifiedPackageTableDecorator class >> browserPluginClass [
	^ClyMonticelloBrowserPlugin 
]

{ #category : #decoration }
ClyModifiedPackageTableDecorator class >> decorateMainTableCell: anItemCellMorph of: aDataSourceItem [
	super decorateMainTableCell: anItemCellMorph of: aDataSourceItem.
	
	anItemCellMorph definitionIcon: #dirtyMonticelloPackageIcon
]

{ #category : #decoration }
ClyModifiedPackageTableDecorator class >> decorateTableCell: anItemCellMorph of: aDataSourceItem [

]

{ #category : #decoration }
ClyModifiedPackageTableDecorator class >> priority [
	^ClyPackageIconTableDecorator priority + 100
]

{ #category : #decoration }
ClyModifiedPackageTableDecorator class >> wantsDecorateTableCellOf: aDataSourceItem [

	^aDataSourceItem isMarkedWith: ClyDirtyPackageTag
]
