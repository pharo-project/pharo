Extension { #name : 'Package' }

{ #category : '*Calypso-SystemTools-Core' }
Package class >> calypsoSystemToolsCoreCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap tableDecorators
		add: ClyEmptyPackageTableDecorator;
		add: ClyPackageIconTableDecorator
]
