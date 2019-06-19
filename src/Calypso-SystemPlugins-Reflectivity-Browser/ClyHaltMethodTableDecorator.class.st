"
I decorate table with special icon for methods with halt or breakpoint
"
Class {
	#name : #ClyHaltMethodTableDecorator,
	#superclass : #ClyMethodTableDecorator,
	#category : #'Calypso-SystemPlugins-Reflectivity-Browser-Breakpoints'
}

{ #category : #decoration }
ClyHaltMethodTableDecorator class >> decorateMainTableCell: anItemCellMorph of: aDataSourceItem [
	
	| icon |
	super decorateMainTableCell: anItemCellMorph of: aDataSourceItem.
	
	icon := anItemCellMorph definitionIcon: #halt.
	icon setBalloonText: 'halt call inside method'
]

{ #category : #decoration }
ClyHaltMethodTableDecorator class >> decorateTableCell: anItemCellMorph of: aDataSourceItem [

]

{ #category : #decoration }
ClyHaltMethodTableDecorator class >> priority [
	^900
]

{ #category : #testing }
ClyHaltMethodTableDecorator class >> wantsDecorateTableCellOf: aDataSourceItem [
	^ (aDataSourceItem isMarkedWith: ClyMethodWithHaltTag) 
			and: [ (aDataSourceItem isMarkedWith: ClyMethodWithBreakpointTag) not ]
]
