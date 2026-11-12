"
I am a context of selected variables in full browser
"
Class {
	#name : 'ClyFullBrowserVariableContext',
	#superclass : 'ClyFullBrowserContext',
	#category : 'Calypso-SystemTools-FullBrowser-Contexts',
	#package : 'Calypso-SystemTools-FullBrowser',
	#tag : 'Contexts'
}

{ #category : 'commands' }
ClyFullBrowserVariableContext class >> calypsoSystemToolsFullBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap shortcuts
		add: SycRemoveVariablesCommand key: $x meta;
		add: SycRenameVariableCommand key: $r meta.
	(aCommandMap shortcuts add: SycInspectVariableValueCommand key: $i meta) priority: 100000.
	(aCommandMap doubleClick add: SycInspectVariableValueCommand) priority: 10000.
	aCommandMap contextMenu
		add: SycAbstractVariableCommand;
		add: SycGenerateVariableAccessorCommand;
		add: SycGenerateVariableAccessorsWithLazyInitializationCommand;
		add: SycMergeVariableCommand;
		add: SycMoveInstanceVariableToClassSideCommand;
		add: SycProtectVariableCommand;
		add: SycPushDownVariableCommand;
		add: SycPushUpVariableCommand;
		add: SycRemoveVariablesCommand;
		add: SycRenameVariableCommand.
	(aCommandMap contextMenu group: ClyQueryMenuGroup)
		add: SycBrowseVariableCommand;
		add: SycInspectVariableValueCommand
]

{ #category : 'selection strategy' }
ClyFullBrowserVariableContext class >> selectionStrategy [
	<classAnnotation>

	^(ClyContextSelectionStrategyAnnotation for: ClyFullBrowserMorph selectionType: ClyVariableMethodGroup)
		priority: 1000 "variable context should be with more priority than basic method group context"
]

{ #category : 'selection strategy' }
ClyFullBrowserVariableContext class >> selectionStrategy2 [
	<classAnnotation>

	^(ClyContextSelectionStrategyAnnotation for: ClyFullBrowserMorph selectionType: ClyVariable)
]

{ #category : 'testing' }
ClyFullBrowserVariableContext >> isFullBrowserVariableContext [

	^ true
]

{ #category : 'selection-variables' }
ClyFullBrowserVariableContext >> isGlobalVariableSelected [
	^false
]

{ #category : 'selection-variables' }
ClyFullBrowserVariableContext >> isVariableSelected [
	^self hasSelectedItems
]

{ #category : 'selection-variables' }
ClyFullBrowserVariableContext >> lastSelectedVariable [
	^self lastSelectedObject variable
]

{ #category : 'selection-variables' }
ClyFullBrowserVariableContext >> selectedVariables [
	^self selectedObjects collect: [ :each | each variable ]
]
