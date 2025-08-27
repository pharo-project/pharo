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

{ #category : 'activation' }
ClapCommandLineHandler >> activate [
	[
		ClapContext withPragmaCommands
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

	self flag: 'TOREMOVE ance clap is used as default command handler'.
	Smalltalk isInteractive ifFalse: [ ^ exit pass ].

	exit isSuccess ifFalse: [ ^ exit resignalAs: (Error new messageText: exit messageText) ]
]
