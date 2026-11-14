Extension { #name : 'ClyMethod' }

{ #category : '*Calypso-SystemTools-Core' }
ClyMethod class >> calypsoSystemToolsCoreCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap tabActions
		add: SycCopyMethodNameToClypboardCommand;
		add: SycShowMethodVersionCommand order: 1000.
	aCommandMap shortcuts
		add: SycMoveMethodsToClassSideCommand key: $t meta , $c meta;
		add: SycMoveMethodsToInstanceSideCommand key: $t meta , $i meta;
		addMenuOf: SycMethodRefactoringMenuGroup key: $t meta;
		add: SycPullUpMethodCommand key: $p meta , $u meta;
		add: SycPushDownMethodCommand key: $p meta , $d meta;
		add: SycRenameMessageCommand key: $r meta;
		add: SycShowMethodVersionCommand key: $v meta.
	aCommandMap dragAndDrop
		add: SycMoveMethodsToClassCommand dropIn: ClyClass;
		add: SycMoveMethodsToPackageCommand dropIn: Package.
	aCommandMap tabs add: ClyMethodCodeEditorToolMorph
]

{ #category : '*Calypso-SystemTools-Core' }
ClyMethod class >> systemCommandsMethodCommandsIn: aCommandMap [

	<calypsoCommands>
	(aCommandMap contextMenu group: SycMethodRefactoringMenuGroup)
		add: SycPullUpMethodCommand order: 10;
		add: SycPushDownMethodCommand order: 20;
		add: SycPushDownMethodInSomeClassesCommand order: 30;
		add: SycFindAndReplaceMethodCommand order: 40;
		add: SycAddMessageArgumentCommand order: 50;
		add: SycInlineIntoLocalSendersMessageCommand order: 60;
		add: SycRemoveAllSendersCommand order: 70;
		add: SycRemoveMessageArgumentCommand order: 80;
		add: SycMoveMethodsToClassSideCommand order: 90;
		add: SycMoveMethodsToInstanceSideCommand order: 100;
		add: SycMoveMethodsToClassCommand order: 110;
		add: SycMoveMethodsToPackageCommand order: 120;
		add: SycMoveMethodsToTheDefiningClassCommand order: 130;
		add: SycRemoveMethodsInHierarchyCommand order: 140;
		add: SycReplaceMessageCommand order: 150.
	(aCommandMap contextMenu group: SycMethodExtraMenuGroup)
		add: SycClassifyMethodsCommand order: 10;
		add: SycCopyMethodNameToClipboardCommand order: 20;
		add: SycCopyTestMethodNameToClipboardCommand order: 30;
		add: SycFileOutMethodCommand order: 40;
		add: SycInspectMethodCommand order: 50.
	aCommandMap shortcuts add: SycRemoveMethodCommand key: $x meta
]
