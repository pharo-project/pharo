Extension { #name : 'ClyClass' }

{ #category : '*Calypso-SystemTools-Core' }
ClyClass class >> calypsoSystemToolsCoreCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap shortcuts
		add: SycAbstractAllInstVarAccessorsCommand key: $a meta , $a meta;
		add: SycGenerateAllInstVarAccessorsCommand key: $g meta , $a meta;
		addMenuOf: SycClassRefactoringMenuGroup key: $t meta;
		add: SycRenameClassCommand2 key: $r meta.
	aCommandMap tabs add: ClyClassDefinitionEditorToolMorph.
	aCommandMap tableDecorators
		add: ClyClassIconTableDecorator;
		add: ClyUncommentedItemTableDecorator
]

{ #category : '*Calypso-SystemTools-Core' }
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
