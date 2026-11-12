Extension { #name : 'ClyMethod' }

{ #category : '*Calypso-SystemPlugins-InheritanceAnalysis-Browser' }
ClyMethod class >> calypsoSystemPluginsInheritanceAnalysisBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap shortcuts add: ClyShowMethodInheritanceCommand key: $h meta.
	aCommandMap contextMenu add: ClyShowMethodInheritanceCommand
]
