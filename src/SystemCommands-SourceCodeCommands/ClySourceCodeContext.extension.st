Extension { #name : 'ClySourceCodeContext' }

{ #category : '*SystemCommands-SourceCodeCommands' }
ClySourceCodeContext class >> systemCommandsSourceCodeCommandsIn: aCommandMap [

	<calypsoCommands>
	(aCommandMap contextMenu group: SycSourceCodeMenuGroup)
		add: SycAbstractAllInstVarAccessorsCommand;
		add: SycAbstractVariableCommand;
		add: SycAddMessageArgumentCommand;
		add: SycAddNewProtocolCommand;
		add: SycCategorizeAllUnclassifiedMethodsCommand;
		add: SycConvertTempToInstVarCommand;
		add: SycDeprecateMessageCommand;
		add: SycExtractMethodAndOccurrencesCommand;
		add: SycExtractMethodCommand;
		add: SycExtractSetUpCommand;
		add: SycExtractTempCommand;
		add: SycFindAndReplaceMethodCommand;
		add: SycFormatSourceCodeCommand;
		add: SycGenerateAllInstVarAccessorsCommand;
		add: SycGenerateVariableAccessorCommand;
		add: SycGenerateVariableAccessorsWithLazyInitializationCommand;
		add: SycInlineIntoLocalSendersMessageCommand;
		add: SycInlineMethodCommand;
		add: SycInlineTempCommand;
		add: SycInspectNodeCommand;
		add: SycMergeVariableCommand;
		add: SycMigrateReferencesOfClassCommand;
		add: SycMoveInstanceVariableToClassSideCommand;
		add: SycMoveTempDefinitionCommand;
		add: SycNewExtractMethodCommand;
		add: SycProtectVariableCommand;
		add: SycPushDownVariableCommand;
		add: SycPushUpVariableCommand;
		add: SycRealizeClassCommand;
		add: SycRemoveAllSendersCommand;
		add: SycRemoveClassCommand;
		add: SycRemoveEmptyMethodTagsCommand;
		add: SycRemoveMessageArgumentCommand;
		add: SycRemoveVariablesCommand;
		add: SycRenameArgOrTempCommand;
		add: SycRenameArgOrTempCommand2;
		add: SycRenameClassCommand;
		add: SycRenameClassCommand2;
		add: SycRenameMessageCommand;
		add: SycRenameVariableCommand;
		add: SycReplaceMessageCommand;
		add: SycRunTestCommand;
		add: SycShowLocalSendersCommand.
	(aCommandMap contextMenu group: SycSourceCodeQueryMenuGroup)
		add: SycBrowseClassCommand;
		add: SycBrowseVariableCommand;
		add: SycInspectVariableValueCommand.
	aCommandMap shortcuts
		add: SycExtractMethodAndOccurrencesCommand key: $e meta , $m meta;
		add: SycExtractMethodCommand key: $e meta , $m meta;
		add: SycNewExtractMethodCommand key: $e meta , $m meta , $t meta
]
