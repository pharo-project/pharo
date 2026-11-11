Extension { #name : 'ClyFullBrowserVariableContext' }

{ #category : '*Calypso-SystemTools-QueryBrowser' }
ClyFullBrowserVariableContext class >> calypsoSystemToolsQueryBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap shortcuts
		add: ClyShowVariableRefCommand key: $n meta;
		add: ClyShowVariableWriterCommand key: $m meta
]
