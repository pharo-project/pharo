Extension { #name : 'RBLiteralArrayNode' }

{ #category : '*SystemCommands-SourceCodeCommands' }
RBLiteralArrayNode >> displaySourceCode [
	| code |
	code := super displaySourceCode.
	(code endsWith: '...') ifTrue: [ code := code , ')' ].
	^ code
]
