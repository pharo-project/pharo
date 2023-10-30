"
I decorate table with simple constant icon for projects
"
Class {
	#name : 'ClyProjectIconTableDecorator',
	#superclass : 'ClyTableDecorator',
	#category : 'Calypso-SystemTools-Core-TableDecorators',
	#package : 'Calypso-SystemTools-Core',
	#tag : 'TableDecorators'
}

{ #category : 'decoration' }
ClyProjectIconTableDecorator class >> decorateMainTableCell: anItemCellMorph of: aDataSourceItem [

	super decorateMainTableCell: anItemCellMorph of: aDataSourceItem.

	anItemCellMorph definitionIcon: #packageColored
]

{ #category : 'decoration' }
ClyProjectIconTableDecorator class >> decorateTableCell: anItemCellMorph of: aDataSourceItem [

	
]

{ #category : 'decoration' }
ClyProjectIconTableDecorator class >> decorationStrategy [

	<classAnnotation>
	^ ClyTableDecorationStrategyAnnotation for: Project asCalypsoItemContext
]
