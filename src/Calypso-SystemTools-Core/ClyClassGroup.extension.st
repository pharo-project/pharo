Extension { #name : 'ClyClassGroup' }

{ #category : '*Calypso-SystemTools-Core' }
ClyClassGroup class >> calypsoSystemToolsCoreCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap tableDecorators add: ClyGroupItemTableDecorator
]
