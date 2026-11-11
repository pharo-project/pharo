Extension { #name : 'ClyMethod' }

{ #category : '*Calypso-SystemTools-Core' }
ClyMethod class >> calypsoSystemToolsCoreCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap contextMenu add: SycOpenMethodMenuCommand order: 10100.
	aCommandMap tabActions
		add: SycCopyMethodNameToClypboardCommand;
		add: SycShowMethodVersionCommand order: 1000.
	aCommandMap shortcuts
		add: SycMoveMethodsToClassSideCommand key: $t meta , $c meta;
		add: SycMoveMethodsToInstanceSideCommand key: $t meta , $i meta;
		add: SycOpenDebuggingPointsInMethodMenuCommand key: $t meta;
		add: SycOpenMethodMenuCommand key: $t meta;
		add: SycPullUpMethodCommand key: $p meta , $u meta;
		add: SycPushDownMethodCommand key: $p meta , $d meta;
		add: SycRenameMessageCommand key: $r meta;
		add: SycShowMethodVersionCommand key: $v meta.
	aCommandMap dragAndDrop
		add: SycMoveMethodsToClassCommand dropIn: ClyClass;
		add: SycMoveMethodsToPackageCommand dropIn: Package
]
