"
I decorate table with special icon for methods which are defined in traits.
Notice that I only apply decoration when selected class is different than method defining trait
"
Class {
	#name : #ClyTraitMethodDecorator,
	#superclass : #ClyMethodTableDecorator,
	#category : #'Calypso-SystemPlugins-Traits-Browser'
}

{ #category : #decoration }
ClyTraitMethodDecorator class >> decorateMainTableCell: anItemCellMorph of: aDataSourceItem [
	super decorateMainTableCell: anItemCellMorph of: aDataSourceItem.
	
	anItemCellMorph definitionIcon: #traitIcon
]

{ #category : #decoration }
ClyTraitMethodDecorator class >> decorateTableCell: anItemCellMorph of: aDataSourceItem [

]

{ #category : #decoration }
ClyTraitMethodDecorator class >> priority [
	^200
]

{ #category : #testing }
ClyTraitMethodDecorator class >> wantsDecorateTableCellInContext: methodItemContext [
	"Idea is to decorate trait methods only when browser do not selects their origin"
	| definingClass |
	(super wantsDecorateTableCellInContext: methodItemContext) ifFalse: [ ^false ].
	
	methodItemContext isClassSelected ifFalse: [ ^false ].
	definingClass := methodItemContext selectedMethodItems last systemDefinition definingClassItem.
	
	(definingClass isMarkedWith: ClyTraitTag) ifFalse: [ ^false ].
	 
	^methodItemContext lastSelectedClass ~~ definingClass actualObject
]
