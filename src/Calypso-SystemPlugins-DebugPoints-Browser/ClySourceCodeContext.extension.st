Extension { #name : 'ClySourceCodeContext' }

{ #category : '*Calypso-SystemPlugins-DebugPoints-Browser' }
ClySourceCodeContext class >> calypsoSystemPluginsDebugPointsBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap contextMenu add: SycOpenDebuggingPointsMenuCommand order: 0.2
]
