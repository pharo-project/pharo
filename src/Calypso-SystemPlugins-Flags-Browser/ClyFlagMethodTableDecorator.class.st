"
I decorate methods with flag in the table with special icon
"
Class {
	#name : #ClyFlagMethodTableDecorator,
	#superclass : #ClyMethodTableDecorator,
	#category : #'Calypso-SystemPlugins-Flags-Browser'
}

{ #category : #decoration }
ClyFlagMethodTableDecorator class >> decorateMainTableCell: anItemCellMorph of: aDataSourceItem [
	
	| icon |
	super decorateMainTableCell: anItemCellMorph of: aDataSourceItem.
	
	icon := anItemCellMorph definitionIcon: #flagIcon.
	icon setBalloonText: 'Method has a flag'
]

{ #category : #decoration }
ClyFlagMethodTableDecorator class >> decorateTableCell: anItemCellMorph of: aDataSourceItem [
	
]

{ #category : #decoration }
ClyFlagMethodTableDecorator class >> priority [
	^900
]

{ #category : #testing }
ClyFlagMethodTableDecorator class >> wantsDecorateTableCellOf: aDataSourceItem [
	
	^aDataSourceItem isMarkedWith: ClyMethodFlag 
]
