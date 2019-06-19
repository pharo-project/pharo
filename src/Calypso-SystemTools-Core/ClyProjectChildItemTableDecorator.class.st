Class {
	#name : #ClyProjectChildItemTableDecorator,
	#superclass : #ClyTableDecorator,
	#category : #'Calypso-SystemTools-Core-TableDecorators'
}

{ #category : #decoration }
ClyProjectChildItemTableDecorator class >> decorateMainTableCell: anItemCellMorph inContext: itemContext [

	itemContext lastSelectedItem actualObject
		decorateTableCell: anItemCellMorph inContext: itemContext
]

{ #category : #decoration }
ClyProjectChildItemTableDecorator class >> decorateTableCell: anItemCellMorph of: groupItem [

]

{ #category : #decoration }
ClyProjectChildItemTableDecorator class >> decorationStrategy [
	<classAnnotation>
	
	^ClyTableDecorationStrategy for: ClyProjectChildItem asCalypsoItemContext
]
