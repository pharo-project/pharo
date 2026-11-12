Extension { #name : 'ClyClass' }

{ #category : '*Calypso-SystemTools-Core' }
ClyClass class >> calypsoSystemToolsCoreCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap shortcuts
		add: SycAbstractAllInstVarAccessorsCommand key: $a meta , $a meta;
		add: SycGenerateAllInstVarAccessorsCommand key: $g meta , $a meta;
		addMenuOf: SycClassRefactoringMenuGroup key: $t meta;
		add: SycRenameClassCommand2 key: $r meta
]
