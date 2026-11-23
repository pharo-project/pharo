Extension { #name : 'ClyMethod' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
ClyMethod class >> calypsoSystemToolsFullBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap dragAndDrop
		add: ClyMoveMethodsToGroupCommand dropIn: ClyAllMethodGroup;
		add: ClyMoveMethodsToGroupCommand dropIn: ClyExternalPackageMethodGroup;
		add: ClyMoveMethodsToGroupCommand dropIn: ClyMethodsInProtocolGroup
]
