Extension { #name : 'ClyClass' }

{ #category : '*Calypso-Browser' }
ClyClass class >> calypsoBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap contextMenu
		add: SycMigrateReferencesOfClassCommand;
		add: SycRenameClassCommand;
		add: SycRenameClassCommand2.
	(aCommandMap contextMenu group: ClyQueryMenuGroup) add: SycBrowseClassCommand
]
