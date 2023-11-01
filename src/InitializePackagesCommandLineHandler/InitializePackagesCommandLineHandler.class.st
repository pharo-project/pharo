"
Usage: initializePackages  [ --save ]
	
Documentation:
I am a command line to fix some problems in the package manager left by the first step of the Pharo bootstrap.

Examples:
	pharo Pharo.image initializePackages --save
"
Class {
	#name : 'InitializePackagesCommandLineHandler',
	#superclass : 'CommandLineHandler',
	#category : 'InitializePackagesCommandLineHandler',
	#package : 'InitializePackagesCommandLineHandler'
}

{ #category : 'accessing' }
InitializePackagesCommandLineHandler class >> commandName [
	^ 'initializePackages'
]

{ #category : 'accessing' }
InitializePackagesCommandLineHandler class >> description [
	^ 'I am a command line to fix some problems in the package manager left by the first step of the Pharo bootstrap.'
]

{ #category : 'activation' }
InitializePackagesCommandLineHandler >> activate [

	self activateHelp ifTrue: [ ^ self ].

	self packageOrganizer basicBootstrapInitialize.

	(self hasOption: 'save') ifTrue: [ Smalltalk snapshot: true andQuit: false ].
	self exitSuccess
]

