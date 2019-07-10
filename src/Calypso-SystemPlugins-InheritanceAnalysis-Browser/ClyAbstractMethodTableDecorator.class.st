"
I decorate table with special italic font and icon for abstract classes (with #subclassResponsibility inside).
I am activated only in browser with installed ClyInheritanceAnalysisBrowserPlugin
"
Class {
	#name : #ClyAbstractMethodTableDecorator,
	#superclass : #ClyMethodTableDecorator,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Browser'
}

{ #category : #decoration }
ClyAbstractMethodTableDecorator class >> browserPluginClass [
	^ClyInheritanceAnalysisBrowserPlugin 
]

{ #category : #decoration }
ClyAbstractMethodTableDecorator class >> decorateMainTableCell: anItemCellMorph of: aDataSourceItem [
	super decorateMainTableCell: anItemCellMorph of: aDataSourceItem.
	
	anItemCellMorph definitionIcon: #abstractIcon
]

{ #category : #decoration }
ClyAbstractMethodTableDecorator class >> decorateTableCell: anItemCellMorph of: aDataSourceItem [
	| nameMorph |
		
	nameMorph := anItemCellMorph label.
	nameMorph clyAddEmphasis: TextEmphasis italic.
	nameMorph color: (nameMorph color contrastingColorAdjustment) contrastingColorAdjustment
]

{ #category : #decoration }
ClyAbstractMethodTableDecorator class >> priority [
	^700
]

{ #category : #testing }
ClyAbstractMethodTableDecorator class >> wantsDecorateTableCellOf: aDataSourceItem [
	^aDataSourceItem isMarkedWith: ClyAbstractItemTag
]
