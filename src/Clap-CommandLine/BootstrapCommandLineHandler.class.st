"
Clap commands are found though pragmas defined on commands.
When we bootstrap Pharo, pragmas does not work until we load the compiler.
But Hermes Clap command is needed before to load the compiler.
That's why we define a specific bootstrap command line handler that will be activated only when HermesClapCommand is present in the image and Clap commands are not found.
"
Class {
	#name : 'BootstrapCommandLineHandler',
	#superclass : 'ClapCommandLineHandler',
	#category : 'Clap-CommandLine',
	#package : 'Clap-CommandLine'
}

{ #category : 'handler selection' }
BootstrapCommandLineHandler class >> isResponsibleFor: aCommandLineArguments [

	aCommandLineArguments arguments ifEmpty: [ ^ false ].
	(aCommandLineArguments arguments first = BasicHermesCommandLineHandler commandName)
		ifFalse: [ ^ false "not a loadHermes command" ].
	
	^ (super isResponsibleFor: aCommandLineArguments) not "command not found because of not-working pragmas"
		and: [ Smalltalk globals hasClassNamed: #HermesClapCommand ]
]

{ #category : 'activation' }
BootstrapCommandLineHandler >> activate [

	self activateWith: { (Smalltalk globals at: #HermesClapCommand) commandSpecification }
]
