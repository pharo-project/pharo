"
I am a context of selected method groups in full browser
"
Class {
	#name : 'ClyFullBrowserMethodGroupContext',
	#superclass : 'ClyFullBrowserContext',
	#category : 'Calypso-SystemTools-FullBrowser-Contexts',
	#package : 'Calypso-SystemTools-FullBrowser',
	#tag : 'Contexts'
}

{ #category : 'commands' }
ClyFullBrowserMethodGroupContext class >> calypsoSystemToolsFullBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap shortcuts
		add: ClyRemoveMethodGroupCommand key: $x meta;
		add: ClyRenameProtocolCommand key: $r meta.
	aCommandMap contextMenu
		add: ClyConvertMethodGroupToProtocolCommand;
		add: ClyMoveMethodGroupsToPackageCommand;
		add: ClyRemoveMethodGroupCommand;
		add: ClyRenameProtocolCommand;
		add: SycAbstractAllInstVarAccessorsCommand;
		add: SycAddNewProtocolCommand;
		add: SycCategorizeAllUnclassifiedMethodsCommand;
		add: SycGenerateAllInstVarAccessorsCommand;
		add: SycRealizeClassCommand;
		add: SycRemoveClassCommand;
		add: SycRemoveEmptyMethodTagsCommand
]

{ #category : 'selection strategy' }
ClyFullBrowserMethodGroupContext class >> selectionStrategy [
	<classAnnotation>

	^ClyContextSelectionStrategyAnnotation for: ClyFullBrowserMorph selectionType: ClyMethodGroup
]

{ #category : 'testing' }
ClyFullBrowserMethodGroupContext >> isFullBrowserMethodGroupContext [

	^ true
]

{ #category : 'selection-method groups' }
ClyFullBrowserMethodGroupContext >> selectedMethodGroupItems [
	^selectedItems
]
