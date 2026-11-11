Extension { #name : 'ClySourceCodeContext' }

{ #category : '*SystemCommands-SourceCodeCommands' }
ClySourceCodeContext class >> systemCommandsSourceCodeCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap shortcuts
		add: SycExtractMethodAndOccurrencesCommand key: $e meta , $m meta;
		add: SycExtractMethodCommand key: $e meta , $m meta;
		add: SycNewExtractMethodCommand key: $e meta , $m meta , $t meta
]
