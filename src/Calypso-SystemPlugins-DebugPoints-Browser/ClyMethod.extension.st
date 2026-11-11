Extension { #name : 'ClyMethod' }

{ #category : '*Calypso-SystemPlugins-DebugPoints-Browser' }
ClyMethod class >> calypsoSystemPluginsDebugPointsBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap contextMenu add: SycOpenDebuggingPointsInMethodMenuCommand order: 1.4
]
