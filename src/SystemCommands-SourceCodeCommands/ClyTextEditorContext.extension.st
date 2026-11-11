Extension { #name : 'ClyTextEditorContext' }

{ #category : '*SystemCommands-SourceCodeCommands' }
ClyTextEditorContext class >> systemCommandsSourceCodeCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap shortcuts add: SycRunTestCommand key: $e meta , $t meta
]
