"
I decorate table with special italic font for abstract classes.
I am activated only in browser with installed ClyInheritanceAnalysisBrowserPlugin
"
Class {
	#name : #ClyAbstractClassTableDecorator,
	#superclass : #ClyClassTableDecorator,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Browser'
}

{ #category : #decoration }
ClyAbstractClassTableDecorator class >> browserPluginClass [
	^ClyInheritanceAnalysisBrowserPlugin 
]

{ #category : #decoration }
ClyAbstractClassTableDecorator class >> decorateTableCell: anItemCellMorph of: aDataSourceItem [
	| nameMorph |
		
	nameMorph := anItemCellMorph label.
	nameMorph emphasis: TextEmphasis italic emphasisCode.
	nameMorph color: (nameMorph color contrastingColorAdjustment) contrastingColorAdjustment
]

{ #category : #testing }
ClyAbstractClassTableDecorator class >> wantsDecorateTableCellOf: aDataSourceItem [
	^aDataSourceItem isMarkedWith: ClyAbstractItemTag
]
