Extension { #name : 'ClyMethodSourceCodeContext' }

{ #category : '*Calypso-SystemPlugins-DebugPoints-Browser' }
ClyMethodSourceCodeContext class >> calypsoSystemPluginsDebugPointsBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	(aCommandMap contextMenu group: ClySourceCodeDebugPointsMenuGroup)
		add: ClyAddBreakPointCommand order: 1;
		add: ClyAddWatchDebugPointCommand order: 4.
	(aCommandMap textLeftBarMenu group: ClyDebuggingPointsMenuGroup)
		add: ClyAddBreakPointCommand order: 1;
		add: ClyAddWatchDebugPointCommand order: 4.
	aCommandMap textLeftBarDoubleClick add: ClyAddBreakPointCommand
]
