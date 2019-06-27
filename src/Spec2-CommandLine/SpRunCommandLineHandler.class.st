"
I execute a Mate application.

Usage: run [--list] <applicationName>
 
	list    lists all applications contained in image,
	
Example:

	pharo Pharo.image run HelloWorld
	pharo Pharo.image run IDE
"
Class {
	#name : #SpRunCommandLineHandler,
	#superclass : #CommandLineHandler,
	#instVars : [
		'applicationName'
	],
	#category : #'Spec2-CommandLine'
}

{ #category : #accessing }
SpRunCommandLineHandler class >> commandName [
	^ 'run'
]

{ #category : #activation }
SpRunCommandLineHandler >> activate [
	self activateHelpWithoutArguments ifTrue: [ ^ self ].
	(self hasOption: 'list') ifTrue: [ ^ self listApplications ]. 
	self runApplication
]

{ #category : #activation }
SpRunCommandLineHandler >> applicationName [
	^ applicationName ifNil: [ applicationName := self argumentAt: 1 ]
]

{ #category : #activation }
SpRunCommandLineHandler >> listApplications [ 
	| nl |
	
	nl := Smalltalk platform lineEnding.
	self stdout 
		<< 'List of applications: ' << nl
		<< '---- ' << nl.
	SpApplication findAll do: [ :each | 
		self stdout << each applicationName << nl ].
	
	self exitSuccess
]

{ #category : #activation }
SpRunCommandLineHandler >> runApplication [
	| appClass |
	
	appClass := SpApplication 
		findNamed: self applicationName
		ifNone: [ ^ self exitFailure: ('Application {1} not found.' format: { self applicationName }) ].
	
	[ 
		appClass new 
			prepareAsStandaloneApplication;
			run ] 
	fork
]
