"
I hook Clap into the existing command-line handlers system.
"
Class {
	#name : 'ClapCommandLineHandler',
	#superclass : 'NewCommandLineHandler',
	#category : 'Clap-CommandLine',
	#package : 'Clap-CommandLine'
}

{ #category : 'class initialization' }
ClapCommandLineHandler class >> initialize [
	
	self registerAsCommandLinehandler
]

{ #category : 'activation' }
ClapCommandLineHandler >> activateCommand [

	self isInteractiveAndNoArguments ifTrue: [ ^ self ].
	
	ClapContext executeWithPragmaCommandsAndArguments: self commandLine arguments
]
