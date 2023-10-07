Extension { #name : 'RBMessageNode' }

{ #category : '*SystemCommands-SourceCodeCommands' }
RBMessageNode >> displaySourceCode [
	^ selector value asString asSymbol
]
