"
I decorate table of critiques with severity level icon
"
Class {
	#name : #ClyCritiquesTableDecorator,
	#superclass : #ClyTableDecorator,
	#category : #'Calypso-SystemPlugins-Critic-Browser'
}

{ #category : #decoration }
ClyCritiquesTableDecorator class >> browserPluginClass [
	^ClyCriticBrowserPlugin 
]

{ #category : #decoration }
ClyCritiquesTableDecorator class >> decorateMainTableCell: anItemCellMorph of: aDataSourceItem [
	super decorateMainTableCell: anItemCellMorph of: aDataSourceItem.
		
	anItemCellMorph definitionIcon: (aDataSourceItem getProperty: ClyCritiqueSeverityProperty) iconName
]

{ #category : #decoration }
ClyCritiquesTableDecorator class >> decorateTableCell: anItemCellMorph of: aDataSourceItem [

	anItemCellMorph color: (aDataSourceItem position odd  
		ifTrue:  [ Smalltalk ui theme backgroundColor ]
		ifFalse: [ Smalltalk ui theme lightBackgroundColor ]).

]

{ #category : #decoration }
ClyCritiquesTableDecorator class >> decorationStrategy [
	<classAnnotation>
	
	^ClyTableDecorationStrategy for: ReAbstractCritique asCalypsoItemContext 
]
