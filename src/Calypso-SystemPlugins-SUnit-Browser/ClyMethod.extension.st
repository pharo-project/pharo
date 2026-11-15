Extension { #name : 'ClyMethod' }

{ #category : '*Calypso-SystemPlugins-SUnit-Browser' }
ClyMethod class >> calypsoSystemPluginsSUnitBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap contextMenu
		add: ClyDebugTestCommand;
		add: ClyDebugTestSetupCommand;
		add: ClyRunCoveringTestMethodsCommand;
		add: ClyRunTestsFromMethodsCommand.
	(aCommandMap tableIcons add: ClyRunCoveringTestMethodsCommand) priority: 1000.
	(aCommandMap tableIcons add: ClyRunTestsFromMethodsCommand) priority: 1000.
	aCommandMap shortcuts
		add: ClyDebugTestCommand key: $d meta;
		add: ClyDebugTestSetupCommand key: $d shift meta;
		add: ClyJumpToTestMethodCommand key: $h meta , $j meta;
		add: ClyOpenTestMethodCommand key: $t meta , $o meta;
		add: ClyRunCoveringTestMethodsCommand key: $t meta;
		add: ClyRunTestsFromMethodsCommand key: $t meta.
	(aCommandMap contextMenu group: ClySUnitMethodMenuGroup)
		add: ClyJumpToTestMethodCommand;
		add: ClyOpenTestMethodCommand
]
