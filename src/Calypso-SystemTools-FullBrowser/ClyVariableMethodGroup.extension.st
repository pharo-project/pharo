Extension { #name : 'ClyVariableMethodGroup' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
ClyVariableMethodGroup class >> calypsoSystemToolsFullBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	(aCommandMap contextMenu group: ClyMethodGroupExtraMenuGroup)
		add: SycInspectVariableCommand order: 10;
		add: ClyInvertMethodVisibilityHierarchyCommand order: 20;
		add: ClyInvertVariableOrderCommand order: 30
]
