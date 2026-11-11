"
I am a tool context used as an anchor by the command registration tests. I get the commands declared on my superclass and declare my own.
"
Class {
	#name : 'ClyMockToolSubContext',
	#superclass : 'ClyMockToolContext',
	#category : 'Calypso-Commands-Tests',
	#package : 'Calypso-Commands-Tests'
}

{ #category : 'commands' }
ClyMockToolSubContext class >> mockSubCommandsIn: aCommandMap [

	<calypsoCommands>
	(aCommandMap contextMenu add: ClyMockRegisteredSecondCommand) label: 'Second from the subcontext'.
	aCommandMap toolbar addAllConcreteSubclassesOf: ClyMockRegisteredCommand
]
