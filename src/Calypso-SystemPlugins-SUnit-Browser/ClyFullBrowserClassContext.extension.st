Extension { #name : 'ClyFullBrowserClassContext' }

{ #category : '*Calypso-SystemPlugins-SUnit-Browser' }
ClyFullBrowserClassContext class >> calypsoSystemPluginsSUnitBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap shortcuts add: ClyJumpToTestClassCommand key: $g meta , $j meta
]
