Extension { #name : 'OCMessageNode' }

{ #category : '*SystemCommands-SourceCodeCommands' }
OCMessageNode >> displaySourceCode [
	^ selector value asString asSymbol
]
