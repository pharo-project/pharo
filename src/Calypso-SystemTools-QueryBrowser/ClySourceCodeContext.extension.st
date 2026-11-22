Extension { #name : 'ClySourceCodeContext' }

{ #category : '*Calypso-SystemTools-QueryBrowser' }
ClySourceCodeContext class >> calypsoSystemToolsQueryBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	(aCommandMap contextMenu group: SycSourceCodeQueryMenuGroup)
		add: ClyShowClassRefCommand;
		add: ClyShowMessageImplementorCommand;
		add: ClyShowMessageSenderCommand;
		add: ClyShowPoolUsersCommand;
		add: ClyShowTraitUsersCommand;
		add: ClyShowVariableRefCommand;
		add: ClyShowVariableWriterCommand
]
