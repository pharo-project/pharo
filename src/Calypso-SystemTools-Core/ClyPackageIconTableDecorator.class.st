"
I decorate table with simple constant icon for packages
"
Class {
	#name : #ClyPackageIconTableDecorator,
	#superclass : #ClyPackageTableDecorator,
	#category : #'Calypso-SystemTools-Core-TableDecorators'
}

{ #category : #decoration }
ClyPackageIconTableDecorator class >> decorateMainTableCell: anItemCellMorph of: aDataSourceItem [
	super decorateMainTableCell: anItemCellMorph of: aDataSourceItem.
	
	anItemCellMorph definitionIcon: #packageIcon
]

{ #category : #decoration }
ClyPackageIconTableDecorator class >> decorateTableCell: anItemCellMorph of: aDataSourceItem [

]
