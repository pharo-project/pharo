"
I decorate table with special double icon (up and down arrows) for overridden and overriding methods.
I add two or one iconic buttons as single icon which allow to open overridden or overriding methods for selected item.

I am activated only in browser with installed ClyInheritanceAnalysisBrowserPlugin
"
Class {
	#name : #ClyOverriddenMethodTableDecorator,
	#superclass : #ClyMethodTableDecorator,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Browser'
}

{ #category : #decoration }
ClyOverriddenMethodTableDecorator class >> browserPluginClass [
	^ClyInheritanceAnalysisBrowserPlugin 
]

{ #category : #decoration }
ClyOverriddenMethodTableDecorator class >> createMultiIconMorph [

	^Morph new 
		changeTableLayout;
		listDirection: #topToBottom;
		listCentering: #center;
		wrapCentering: #center;
		extent: 16@16;
		color: Color transparent
]

{ #category : #decoration }
ClyOverriddenMethodTableDecorator class >> decorateMainTableCell: anItemCellMorph inContext: itemContext [ 
	
	| commands |
	commands := OrderedCollection new: 2.
	(itemContext lastSelectedItem isMarkedWith: ClyOverriddenMethodTag) ifTrue: [ 
		commands add: ClyShowOverridingMethodsCommand new].
	(itemContext lastSelectedItem isMarkedWith: ClyOverridingMethodTag) ifTrue: [ 
		commands add: ClyShowOverriddenMethodsCommand new].
	
	self decorateTableCell: anItemCellMorph withCommands: commands inContext: itemContext
]

{ #category : #decoration }
ClyOverriddenMethodTableDecorator class >> decorateTableCell: anItemCellMorph of: aDataSourceItem [
]

{ #category : #decoration }
ClyOverriddenMethodTableDecorator class >> decorateTableCell: anItemCellMorph withCommands: commands inContext: itemContext [

	| panel activator button |
	panel := self createMultiIconMorph.
	commands do: [ :each | 
		activator := CmdCommandActivator new 
			command: each; 
			context: itemContext.
		button := each createTableCellButtonUsing: activator.
		button hResizing: #spaceFill.
		button vResizing: #spaceFill.
		panel addMorph: button].
	anItemCellMorph definitionMorph: panel

]

{ #category : #decoration }
ClyOverriddenMethodTableDecorator class >> priority [
	^800
]

{ #category : #testing }
ClyOverriddenMethodTableDecorator class >> wantsDecorateTableCellOf: aDataSourceItem [

	^(aDataSourceItem isMarkedWith: ClyOverridingMethodTag)
		or: [ aDataSourceItem isMarkedWith: ClyOverriddenMethodTag ]
]
