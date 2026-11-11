Extension { #name : 'ClyMethod' }

{ #category : '*SystemCommands-MethodCommands' }
ClyMethod class >> systemCommandsMethodCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap contextMenu add: SycOpenExtraInMethodMenuCommand order: 11200.
	aCommandMap shortcuts add: SycRemoveMethodCommand key: $x meta
]
