"
I hook Clap into the existing command-line handlers system.
"
Class {
	#name : #ClapCommandLineHandler,
	#superclass : #CommandLineHandler,
	#category : #'Clap-CommandLine'
}

{ #category : #accessing }
ClapCommandLineHandler class >> commandName [
	^ 'clap'
]

{ #category : #activation }
ClapCommandLineHandler >> activate [
	[
		ClapContext withPragmaCommands beObeyingExits
			activateWith: self arguments
			onExit: [ :exit | self handleExit: exit ]
	]
	forkAt: Processor userSchedulingPriority
	named: 'Clap commandline handler process'
]

{ #category : #activation }
ClapCommandLineHandler >> handleExit: exit [
	Smalltalk isInteractive
		ifFalse: [ ^ exit pass ].

	exit isSuccess
		ifFalse: [ ^ exit resignalAs: (Error new messageText: exit messageText) ]
]

{ #category : #activation }
ClapCommandLineHandler >> noneMatched [
	self exitFailure: 'Unknown command'
]
