"
I decorate table with special icon for ffi methods
"
Class {
	#name : #ClyFFIMethodTableDecorator,
	#superclass : #ClyMethodTableDecorator,
	#category : #'Calypso-SystemPlugins-FFI-Browser'
}

{ #category : #decoration }
ClyFFIMethodTableDecorator class >> decorateMainTableCell: anItemCellMorph of: aDataSourceItem [
	super decorateMainTableCell: anItemCellMorph of: aDataSourceItem.
	
	anItemCellMorph definitionIcon: #recoverLostChanges
]

{ #category : #decoration }
ClyFFIMethodTableDecorator class >> decorateTableCell: anItemCellMorph of: aDataSourceItem [
]

{ #category : #testing }
ClyFFIMethodTableDecorator class >> wantsDecorateTableCellOf: aDataSourceItem [
	^aDataSourceItem isMarkedWith: ClyFFIMethodTag 
]
