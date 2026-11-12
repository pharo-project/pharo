Extension { #name : 'ClyTextEditorContext' }

{ #category : '*Calypso-SystemTools-Core' }
ClyTextEditorContext class >> calypsoSystemToolsCoreCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap shortcuts
		add: ClyRequestMethodPackageForEditorCommand key: $e meta , $m meta , $p meta;
		add: ClyRequestProtocolForEditorCommand key: $e meta , $p meta;
		add: ClyResetProtocolForEditorCommand key: $e meta , $u meta;
		add: SycFormatSourceCodeCommand key: $f meta shift
]

{ #category : '*Calypso-SystemTools-Core' }
ClyTextEditorContext class >> systemCommandsSourceCodeCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap shortcuts add: SycRunTestCommand key: $e meta , $t meta
]
