"
I decorate tables in the full browser with special icon for methods and protocol which are named like ""see class side"".
The icon switches the browser to the class side
"
Class {
	#name : #ClyClassSideLinkTableDecorator,
	#superclass : #ClyTableDecorator,
	#category : #'Calypso-SystemTools-Core-TableDecorators'
}

{ #category : #decoration }
ClyClassSideLinkTableDecorator class >> decorateMainTableCell: anItemCellMorph inContext: itemContext [
	| command activator icon button |
	super decorateMainTableCell: anItemCellMorph inContext: itemContext.
	
	command := ClySwitchToClassSideCommand new.
	itemContext prepareNewCommand: command.
	activator := CmdCommandActivator new 
			command: command; 
			context: itemContext.
	icon := anItemCellMorph iconNamed: #forward.
	button := command createTableCellButtonWith: icon using: activator.
	button setBalloonText: 'Switch to the class side'.
	anItemCellMorph definitionMorph: button
]

{ #category : #decoration }
ClyClassSideLinkTableDecorator class >> decorateTableCell: anItemCellMorph of: aDataSourceItem [

]

{ #category : #decoration }
ClyClassSideLinkTableDecorator class >> methodDecorationStrategy [
	<classAnnotation>
	
	^ClyTableDecorationStrategy for: ClyMethodContextOfFullBrowser 
]

{ #category : #decoration }
ClyClassSideLinkTableDecorator class >> methodGroupDecorationStrategy [
	<classAnnotation>
	
	^ClyTableDecorationStrategy for: ClyTaggedMethodGroup asCalypsoItemContext
]

{ #category : #decoration }
ClyClassSideLinkTableDecorator class >> priority [
	^100000
]

{ #category : #decoration }
ClyClassSideLinkTableDecorator class >> wantsDecorateTableCellOf: aDataSourceItem [
	
	| tagName |
	tagName := aDataSourceItem name asLowercase.	
	^#('see class side' 'seeclassside') 
		anySatisfy: [ :each | tagName beginsWith: each ]
]
