"
A Clap command that can be reused to provide a help command.

If topic is specified, search in parent subcommands to find a command matching the topic and display help on this subcommand.
Else, display the current command help.
"
Class {
	#name : 'ClapHelpCommand',
	#superclass : 'ClapApplication',
	#category : 'Clap-Core-Command',
	#package : 'Clap-Core',
	#tag : 'Command'
}

{ #category : 'accessing' }
ClapHelpCommand class >> specification [

	^ (ClapCommandSpec id: #helpCommand)
		description: 'Prints command documentation';
		commandClass: self;
		canonicalName: 'help';
		addFlagWithPositional: #topic
			description: 'The subcommand to document (defaults to the current one)';
		yourself
]

{ #category : 'finding/searching' }
ClapHelpCommand >> commandForTopic: aTopic [
	
	^ self commandParentSpecification 
		subcommandNamed: aTopic
		ifNone: nil
]

{ #category : 'private' }
ClapHelpCommand >> commandParentSpecification [

	^ context lastSubcommand parent specification
]

{ #category : 'execution' }
ClapHelpCommand >> execute [

	| doc topicCommand |
	doc := ClapDocumenter on: context stdout.
	self topic ifNil: [ ^ doc explain: self commandParentSpecification ].
	
	topicCommand := self commandForTopic: self topic.
	topicCommand
		ifNil: [
				context shouldObeyExit ifTrue: [
					context exitFailure: 'Unknown subcommand: ' , self topic ] ]
		ifNotNil: [ doc explain: topicCommand ]
]

{ #category : 'accessing' }
ClapHelpCommand >> topic [
	
	^ [ self positional: #topic ]
		on: NotFound 
		do: [ nil ]
]
