Extension { #name : 'ClyMethodGroup' }

{ #category : '*Calypso-SystemTools-Core' }
ClyMethodGroup class >> calypsoSystemToolsCoreCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap tableDecorators add: ClyGroupItemTableDecorator
]
