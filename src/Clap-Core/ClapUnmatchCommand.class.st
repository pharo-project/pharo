"
Error raised when Clap did not succeed to matcjh a clap command.
"
Class {
	#name : 'ClapUnmatchCommand',
	#superclass : 'Error',
	#instVars : [
		'clapContext'
	],
	#category : 'Clap-Core-Activation',
	#package : 'Clap-Core',
	#tag : 'Activation'
}

{ #category : 'instance creation' }
ClapUnmatchCommand class >> clapContext: aClapContext [

	^ self new
		clapContext: aClapContext;
		yourself
]

{ #category : 'signaling' }
ClapUnmatchCommand class >> signalClapContext: aClapContext [

	^ (self clapContext: aClapContext) signal
]

{ #category : 'accessing' }
ClapUnmatchCommand >> availableCommands [ 

	| subcommands |
	subcommands := clapContext specification subcommands collect: #identifier.
	^ 'Available commands: ', (', ' join: subcommands )
]

{ #category : 'accessing' }
ClapUnmatchCommand >> clapContext: aClapContext [

	clapContext := aClapContext


]

{ #category : 'accessing' }
ClapUnmatchCommand >> contextArguments [ 

	^ 'Context arguments: ', clapContext arguments asString
]

{ #category : 'accessing' }
ClapUnmatchCommand >> messageText [

	^ self contextArguments, String lf, self availableCommands 
]
