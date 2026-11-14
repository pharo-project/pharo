Extension { #name : 'ClyFullBrowserVariableContext' }

{ #category : '*Calypso-SystemPlugins-DebugPoints-Browser' }
ClyFullBrowserVariableContext class >> calypsoSystemPluginsDebugPointsBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	(aCommandMap contextMenu group: ClyDebuggingPointsMenuGroup)
		add: ClyBreakOnVariableReadCommand;
		add: ClyBreakOnVariableReadWriteCommand;
		add: ClyBreakOnVariableWriteCommand;
		add: ClyDebugBreakOnVariableCommand;
		add: ClyDebugVariableCommand;
		add: ClyDebugWatchOnVariableCommand;
		add: ClyDebugWatchOnVariableReadCommand;
		add: ClyDebugWatchOnVariableReadWriteCommand;
		add: ClyDebugWatchOnVariableWriteCommand
]
