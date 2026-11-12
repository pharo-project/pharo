Extension { #name : 'ClyClass' }

{ #category : '*SystemCommands-ClassCommands' }
ClyClass class >> systemCommandsClassCommandsIn: aCommandMap [

	<calypsoCommands>
	(aCommandMap contextMenu group: SycClassRefactoringMenuGroup)
		add: SycMakeAbstractCommand order: 20;
		add: SycDuplicateClassCommand order: 30;
		add: SycDeprecateClassCommand order: 50;
		add: SycGenerateEqualAndHashCommand order: 60;
		add: SycGeneratePrintOnCommand order: 70;
		add: SycInsertSubclassCommand order: 80;
		add: SycInsertSuperclassCommand order: 90.
	(aCommandMap contextMenu group: SycClassExtraMenuGroup)
		add: SycCopyClassNameCommand order: 10;
		add: SycFileOutClassCommand order: 20;
		add: SycInspectClassCommand order: 30;
		add: SycUpdateClassCommand order: 40.
	(aCommandMap contextMenu group: SycClassScopesMenuGroup) add: SycAddClassesScopeCommand order: 10.
	aCommandMap shortcuts add: SycDuplicateClassCommand key: $d meta
]
