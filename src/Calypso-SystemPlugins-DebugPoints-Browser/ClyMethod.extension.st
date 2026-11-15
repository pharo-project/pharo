Extension { #name : 'ClyMethod' }

{ #category : '*Calypso-SystemPlugins-DebugPoints-Browser' }
ClyMethod class >> calypsoSystemPluginsDebugPointsBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	(aCommandMap contextMenu group: ClyMethodDebugPointsMenuGroup)
		add: ClyAddBreakPointCommand order: 1;
		add: ClyAddWatchDebugPointCommand order: 4.
	aCommandMap shortcuts addMenuOf: ClyMethodDebugPointsMenuGroup key: $t meta
]
