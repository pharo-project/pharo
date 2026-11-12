Extension { #name : 'ClyClass' }

{ #category : '*SystemCommands-ClassCommands' }
ClyClass class >> systemCommandsClassCommandsIn: aCommandMap [

	<calypsoCommands>
	(aCommandMap contextMenu group: SycClassRefactoringMenuGroup)
		add: SycCMakeAbstractCommand order: 20;
		add: SycCmDuplicateClassCommand order: 30;
		add: SycCmNewClassCreationCommand order: 40;
		add: SycDeprecateClassCmCommand order: 50;
		add: SycCmGenerateEqualAndHashCommand order: 60;
		add: SycCmGeneratePrintOnCommand order: 70;
		add: SycCmInsertSubclassCommand2 order: 80;
		add: SycCmInsertSuperclassCommand2 order: 90.
	(aCommandMap contextMenu group: SycClassExtraMenuGroup)
		add: SycCmCopyClassNameCommand order: 10;
		add: SycCmFileOutClassCommand order: 20;
		add: SycCmInspectClassCommand order: 30;
		add: SycCmUpdateClassCommand order: 40.
	(aCommandMap contextMenu group: SycClassScopesMenuGroup) add: SycAddClassesScopeCommand order: 10.
	aCommandMap shortcuts add: SycCmDuplicateClassCommand key: $d meta
]
