"
I am a tool context used as an anchor by the command registration tests. My class side declares commands with the <calypsoCommands> pragma.
"
Class {
	#name : 'ClyMockToolContext',
	#superclass : 'ClyToolContext',
	#category : 'Calypso-Commands-Tests',
	#package : 'Calypso-Commands-Tests'
}

{ #category : 'commands' }
ClyMockToolContext class >> mockCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap contextMenu add: ClyMockRegisteredFirstCommand order: 20.
	(aCommandMap contextMenu group: ClyExtraMenuGroup) add: ClyMockRegisteredSecondCommand.
	aCommandMap shortcuts add: ClyMockRegisteredFirstCommand key: $x meta
]

{ #category : 'commands' }
ClyMockToolContext class >> mockTabsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap tabs add: ClyMockTabTool.
	aCommandMap tableDecorators add: ClyMockTableDecorator.
	(aCommandMap tableDecorators add: ClyMockTableDecorator) priority: 1
]
