Extension { #name : 'Project' }

{ #category : '*Calypso-SystemTools-Core' }
Project class >> calypsoSystemToolsCoreCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap tableDecorators add: ClyProjectIconTableDecorator
]
