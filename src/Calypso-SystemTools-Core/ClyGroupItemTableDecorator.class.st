"
I delegate decoration of any group item to the group itself:

	groupItem actualObject decorateTableCell: anItemCellMorph of: groupItem
	
The actualObject is a kind ClyGroup. There are method, class and package groups.

If you introduce new kind of group then to activate this decorator you will need to annotate me with new decoration strategy for new context of this new group
"
Class {
	#name : #ClyGroupItemTableDecorator,
	#superclass : #ClyTableDecorator,
	#category : #'Calypso-SystemTools-Core-TableDecorators'
}

{ #category : #decoration }
ClyGroupItemTableDecorator class >> classGroupDecorationStrategy [
	<classAnnotation>
	
	^ClyTableDecorationStrategy for: ClyClassGroup asCalypsoItemContext
]

{ #category : #decoration }
ClyGroupItemTableDecorator class >> decorateMainTableCell: anItemCellMorph of: groupItem [
	super decorateMainTableCell: anItemCellMorph of: groupItem.
	
	groupItem actualObject decorateTableCell: anItemCellMorph of: groupItem
]

{ #category : #decoration }
ClyGroupItemTableDecorator class >> decorateTableCell: anItemCellMorph of: groupItem [

]

{ #category : #decoration }
ClyGroupItemTableDecorator class >> methodGroupDecorationStrategy [
	<classAnnotation>
	
	^ClyTableDecorationStrategy for: ClyMethodGroup asCalypsoItemContext
]
