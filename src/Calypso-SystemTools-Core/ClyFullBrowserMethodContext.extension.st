Extension { #name : 'ClyFullBrowserMethodContext' }

{ #category : '*Calypso-SystemTools-Core' }
ClyFullBrowserMethodContext class >> calypsoSystemToolsCoreCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap tableDecorators add: ClyClassSideLinkTableDecorator
]
