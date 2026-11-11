Extension { #name : 'ClyClass' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
ClyClass class >> calypsoSystemToolsFullBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap dragAndDrop
		add: ClyMoveClassToGroupCommand dropIn: ClyNoTagClassGroup;
		add: ClyMoveClassToGroupCommand dropIn: ClyTaggedClassGroup;
		add: ClyMoveClassToGroupCommand dropIn: Package
]
