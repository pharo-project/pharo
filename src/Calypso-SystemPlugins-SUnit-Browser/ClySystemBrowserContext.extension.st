Extension { #name : 'ClySystemBrowserContext' }

{ #category : '*Calypso-SystemPlugins-SUnit-Browser' }
ClySystemBrowserContext class >> calypsoSystemPluginsSUnitBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	(aCommandMap contextMenu group: ClyQueryMenuGroup) add: ClyBrowseCoveringTestCaseCommand
]
