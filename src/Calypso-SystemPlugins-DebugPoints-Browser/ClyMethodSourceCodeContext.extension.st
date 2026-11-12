Extension { #name : 'ClyMethodSourceCodeContext' }

{ #category : '*Calypso-SystemPlugins-DebugPoints-Browser' }
ClyMethodSourceCodeContext class >> calypsoSystemPluginsDebugPointsBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	(aCommandMap textLeftBarMenu group: ClyDebuggingPointsMenuGroup)
		add: ClyAddBreakPointCommand order: 1;
		add: ClyAddWatchDebugPointCommand order: 4.
	aCommandMap textLeftBarDoubleClick add: ClyAddBreakPointCommand
]
