Extension { #name : 'OCProgramNode' }

{ #category : '*SystemCommands-SourceCodeCommands' }
OCProgramNode >> displaySourceCode [

	^ (((self sourceCode asString
			 copyReplaceAll: String tab with: String empty)
		    copyReplaceAll: String cr  with: String space)
			 copyReplaceAll: String lf  with: String space)
		  withBlanksCondensed
		  truncateWithElipsisTo: 15
]
