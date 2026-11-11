Extension { #name : 'ClyVariableMethodGroup' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
ClyVariableMethodGroup class >> calypsoSystemToolsFullBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap contextMenu add: SycOpenExtraInMethodGroupMenuCommand order: 10200
]
