Extension { #name : 'ClyMethod' }

{ #category : '*Calypso-SystemTools-QueryBrowser' }
ClyMethod class >> calypsoSystemToolsQueryBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	(aCommandMap toolbar group: ClyButtonToolbarGroup)
		add: ClyShowMessageImplementorCommand order: 1000;
		add: ClyShowMessageSenderCommand order: 1000.
	aCommandMap shortcuts
		add: ClyShowMessageImplementorCommand key: $m meta;
		add: ClyShowMessageSenderCommand key: $n meta
]
