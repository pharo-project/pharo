Extension { #name : 'ClyClass' }

{ #category : '*Calypso-SystemPlugins-InheritanceAnalysis-Browser' }
ClyClass class >> calypsoSystemPluginsInheritanceAnalysisBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap tableDecorators add: ClyAbstractClassTableDecorator
]
