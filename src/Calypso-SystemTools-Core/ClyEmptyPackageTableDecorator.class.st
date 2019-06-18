"
I decorate table with special icon for empty packages which has no classes
"
Class {
	#name : #ClyEmptyPackageTableDecorator,
	#superclass : #ClyPackageTableDecorator,
	#category : #'Calypso-SystemTools-Core-TableDecorators'
}

{ #category : #decoration }
ClyEmptyPackageTableDecorator class >> decorateMainTableCell: anItemCellMorph of: aDataSourceItem [
	super decorateMainTableCell: anItemCellMorph of: aDataSourceItem.
	
	anItemCellMorph definitionIcon: #emptyPackageIcon
]

{ #category : #decoration }
ClyEmptyPackageTableDecorator class >> decorateTableCell: anItemCellMorph of: aDataSourceItem [

]

{ #category : #decoration }
ClyEmptyPackageTableDecorator class >> priority [
	^ClyPackageIconTableDecorator priority + 1
]

{ #category : #testing }
ClyEmptyPackageTableDecorator class >> wantsDecorateTableCellOf: aDataSourceItem [
	^(aDataSourceItem browserItem hasChildrenOf: ClyClass) not
]
