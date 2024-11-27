Extension { #name : 'ASTLiteralArrayNode' }

{ #category : '*SystemCommands-SourceCodeCommands' }
ASTLiteralArrayNode >> displaySourceCode [
	| code |
	code := super displaySourceCode.
	(code endsWith: '...') ifTrue: [ code := code , ')' ].
	^ code
]
