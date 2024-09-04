Extension { #name : 'ASTMessageNode' }

{ #category : '*SystemCommands-SourceCodeCommands' }
ASTMessageNode >> displaySourceCode [
	^ selector value asString asSymbol
]
