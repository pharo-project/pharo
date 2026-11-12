"
I am a context of selected classes in full browser
"
Class {
	#name : 'ClyFullBrowserClassContext',
	#superclass : 'ClyFullBrowserContext',
	#category : 'Calypso-SystemTools-FullBrowser-Contexts',
	#package : 'Calypso-SystemTools-FullBrowser',
	#tag : 'Contexts'
}

{ #category : 'commands' }
ClyFullBrowserClassContext class >> calypsoSystemToolsFullBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	(aCommandMap toolbar group: ClyClassHierarchyToolbarGroup) addAllConcreteSubclassesOf: ClySwitchClassViewModeCommand.
	(aCommandMap toolbar group: ClyMetaLevelToolbarGroup)
		add: ClySwitchToClassSideCommand order: 2;
		add: ClySwitchToInstanceSideCommand order: 1.
	(aCommandMap toolbar group: ClyMethodViewToolbarGroup) addAllConcreteSubclassesOf: ClySwitchMethodViewModeCommand.
	aCommandMap tabActions addMenuOf: SycClassRefactoringMenuGroup.
	aCommandMap shortcuts
		add: ClyMoveClassToGroupCommand key: $m meta , $c meta;
		add: ClySwitchToFullHierarchyCommand key: $h meta;
		add: SycRemoveClassCommand key: $x meta.
	aCommandMap contextMenu
		add: SycAbstractAllInstVarAccessorsCommand;
		add: SycAddNewProtocolCommand;
		add: SycCategorizeAllUnclassifiedMethodsCommand;
		add: SycGenerateAllInstVarAccessorsCommand;
		add: SycRealizeClassCommand;
		add: SycRemoveClassCommand;
		add: SycRemoveEmptyMethodTagsCommand
]

{ #category : 'selection strategy' }
ClyFullBrowserClassContext class >> selectionStrategy [
	<classAnnotation>

	^ClyContextSelectionStrategyAnnotation for: ClyFullBrowserMorph selectionType: ClyClass
]

{ #category : 'testing' }
ClyFullBrowserClassContext >> isFullBrowserClassContext [

	^ true
]

{ #category : 'browser state' }
ClyFullBrowserClassContext >> restoreBrowserState [

	self browser selectClass: self lastSelectedClass
]

{ #category : 'selection-classes' }
ClyFullBrowserClassContext >> selectedClassItems [
	^selectedItems
]
