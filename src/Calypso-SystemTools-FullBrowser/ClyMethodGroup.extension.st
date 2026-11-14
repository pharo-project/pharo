Extension { #name : 'ClyMethodGroup' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
ClyMethodGroup class >> calypsoSystemToolsFullBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap dragAndDrop
		add: ClyMoveMethodGroupToClassCommand dropIn: ClyClass;
		add: ClyMoveMethodGroupsToPackageCommand dropIn: Package
]
