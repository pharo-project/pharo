Extension { #name : 'ClyMethod' }

{ #category : '*SystemCommands-MethodCommands' }
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
