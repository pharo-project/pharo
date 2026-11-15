"
I am a tool context used as an anchor by the tests of the overrides of command declarations. One of my declaration methods overrides the commands declared by the other. Its selector comes first, to show that overrides are applied after all the declarations.
"
Class {
	#name : 'ClyMockOverridingToolContext',
	#superclass : 'ClyToolContext',
	#category : 'Calypso-Commands-Tests',
	#package : 'Calypso-Commands-Tests'
}

{ #category : 'commands' }
ClyMockOverridingToolContext class >> earlyMockOverridesIn: aCommandMap [

	<calypsoCommands>
	aCommandMap contextMenu remove: ClyMockRegisteredSecondCommand.
	aCommandMap contextMenu remove: ClyMockFirstCommand.
	aCommandMap shortcuts rebind: ClyMockRegisteredFirstCommand to: $d meta.
	aCommandMap shortcuts remove: ClyMockRegisteredSecondCommand.
	aCommandMap shortcuts rebindMenuOf: ClyExtraMenuGroup to: $e meta.
	aCommandMap tabActions removeMenuOf: ClyExtraMenuGroup
]

{ #category : 'commands' }
ClyMockOverridingToolContext class >> mockCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap contextMenu add: ClyMockRegisteredFirstCommand.
	(aCommandMap contextMenu group: ClyExtraMenuGroup) add: ClyMockRegisteredSecondCommand.
	aCommandMap shortcuts add: ClyMockRegisteredFirstCommand key: $x meta.
	aCommandMap shortcuts add: ClyMockRegisteredSecondCommand key: $y meta.
	aCommandMap shortcuts addMenuOf: ClyExtraMenuGroup key: $t meta.
	aCommandMap tabActions add: ClyMockRegisteredFirstCommand.
	aCommandMap tabActions addMenuOf: ClyExtraMenuGroup
]
