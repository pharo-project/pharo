"
I hook Clap into the existing command-line handlers system.
"
Class {
	#name : 'ClapCommandLineHandler',
	#superclass : 'CommandLineHandler',
	#category : 'Clap-Core-Pharo-Integration',
	#package : 'Clap-Core',
	#tag : 'Pharo-Integration'
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
