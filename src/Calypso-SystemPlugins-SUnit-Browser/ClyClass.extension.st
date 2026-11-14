Extension { #name : 'ClyClass' }

{ #category : '*Calypso-SystemPlugins-SUnit-Browser' }
ClyClass class >> calypsoSystemPluginsSUnitBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap contextMenu add: ClyRunTestsFromClassesCommand.
	(aCommandMap tableIcons add: ClyRunTestsFromClassesCommand) priority: 1000.
	aCommandMap shortcuts add: ClyRunTestsFromClassesCommand key: $t meta.
	aCommandMap contextMenu add: ClyJumpToTestClassCommand.
	aCommandMap tabs add: ClyTestSetUpEditorToolMorph
]
