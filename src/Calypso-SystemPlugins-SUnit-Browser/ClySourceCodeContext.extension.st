Extension { #name : 'ClySourceCodeContext' }

{ #category : '*Calypso-SystemPlugins-SUnit-Browser' }
ClySourceCodeContext class >> calypsoSystemPluginsSUnitBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	(aCommandMap contextMenu group: SycSourceCodeMenuGroup) add: ClyJumpToTestClassCommand
]
