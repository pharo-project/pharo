Extension { #name : 'ClyClass' }

{ #category : '*Calypso-SystemTools-Core' }
ClyClass class >> calypsoSystemToolsCoreCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap contextMenu add: SycOpenClassMenuCommand order: 10100.
	aCommandMap shortcuts
		add: SycAbstractAllInstVarAccessorsCommand key: $a meta , $a meta;
		add: SycGenerateAllInstVarAccessorsCommand key: $g meta , $a meta;
		add: SycOpenClassMenuCommand key: $t meta;
		add: SycRenameClassCommand2 key: $r meta
]
