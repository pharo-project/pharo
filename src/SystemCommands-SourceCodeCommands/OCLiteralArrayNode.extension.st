Extension { #name : 'OCLiteralArrayNode' }

{ #category : '*SystemCommands-SourceCodeCommands' }
OCLiteralArrayNode >> displaySourceCode [
	| code |
	code := super displaySourceCode.
	(code endsWith: '...') ifTrue: [ code := code , ')' ].
	^ code
]
