"
I hook Clap into the existing command-line handlers system.
"
Class {
	#name : 'ClapCommandLineHandler',
	#superclass : 'CommandLineHandler',
	#category : 'Clap-CommandLine',
	#package : 'Clap-CommandLine'
}

{ #category : 'accessing' }
ClapCommandLineHandler class >> commandName [
	^ 'clap'
]

{ #category : 'handler selection' }
ClapCommandLineHandler class >> isResponsibleFor: aCommandLineArguments [

	| subcommand |
	aCommandLineArguments arguments ifEmpty: [ ^ false ].
	
	subcommand := aCommandLineArguments arguments first.
	^ ClapContext pragmaCommands
		detect: [ :commandSpec | commandSpec hasAlias: subcommand ]
		ifFound: [ true ] 
		ifNone: [ false ]
]

{ #category : 'handler selection' }
ClapCommandLineHandler class >> priority [ 
	"Take priority on basic command line handlers"
	^ 10
]

{ #category : 'activation' }
ClapCommandLineHandler >> activate [
	
	self activateWith: ClapContext pragmaCommands
]

{ #category : 'activation' }
ClapCommandLineHandler >> activateWith: clapCommandSpecifications [
	[
		(ClapContext withAll: clapCommandSpecifications)
			beObeyingExits;
			setStdio: Stdio;
			arguments: self arguments;
			execute
	]
	forkAt: Processor userSchedulingPriority
	named: 'Clap commandline handler process'
]

{ #category : 'activation' }
ClapCommandLineHandler >> handleExit: exit [

	self flag: 'TOREMOVE once clap is used as default command handler'.
	Smalltalk isInteractive ifFalse: [ ^ exit pass ].

	exit isSuccess ifFalse: [ ^ exit resignalAs: (Error new messageText: exit messageText) ]
]
