Extension { #name : 'ClyMethod' }

{ #category : '*Calypso-Browser' }
ClyMethod class >> calypsoBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap contextMenu
		add: SycDeprecateMessageCommand;
		add: SycProfileTestMethodCommand;
		add: SycRemoveMethodCommand;
		add: SycRenameMessageCommand;
		add: SycShowLocalSendersCommand.
	(aCommandMap contextMenu group: ClyQueryMenuGroup) add: SycShowMethodVersionCommand
]
