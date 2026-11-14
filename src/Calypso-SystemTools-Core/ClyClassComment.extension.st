Extension { #name : 'ClyClassComment' }

{ #category : '*Calypso-SystemTools-Core' }
ClyClassComment class >> calypsoSystemToolsCoreCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap tableDecorators add: ClyClassCommentTableDecorator
]
