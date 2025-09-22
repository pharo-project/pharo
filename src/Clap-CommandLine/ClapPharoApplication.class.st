"
I am an abstract class providing some support to Pharo commands available from command-line.
"
Class {
	#name : 'ClapPharoApplication',
	#superclass : 'ClapApplication',
	#category : 'Clap-CommandLine',
	#package : 'Clap-CommandLine'
}

{ #category : 'adding' }
ClapPharoApplication class >> addKeepAliveOptionTo: aCommandSpec [

	aCommandSpec addFlag: #keepAlive description: 'Keep the image running after the command terminates'
]

{ #category : 'adding' }
ClapPharoApplication class >> addNoQuitOptionTo: aCommandSpec [

	aCommandSpec addFlag: #'no-quit' description: '[deprecated] keep the image running without activating any other command line handler'
]

{ #category : 'adding' }
ClapPharoApplication class >> addRenameOptionTo: aCommandSpec [

	aCommandSpec addFlag: #rename description: 'Rename the image as <NAME> before running the command'
]

{ #category : 'adding' }
ClapPharoApplication class >> addSaveFlagTo: aCommandSpec [
	aCommandSpec addFlag: #save description: 'Save the image after the command terminates'
]

{ #category : 'execution' }
ClapPharoApplication >> doNotQuitImage [ 

	self context doNotObeyExits
]

{ #category : 'execution' }
ClapPharoApplication >> execute [

	self prepareImage.
	self shouldQuit ifFalse: [ self doNotQuitImage ]
]

{ #category : 'execution' }
ClapPharoApplication >> prepareImage [
	self positional: #rename ifPresent: [ :rename | 
		| imageFile |
		imageFile := rename value asFileReference.
		self flag: 'check if it already exists & not same as current'.
		Smalltalk image
				changeImagePathTo: imageFile;
				closeSourceFiles;
				openSourceFiles ]
]

{ #category : 'execution' }
ClapPharoApplication >> saveSessionIfAsked [

	self shouldSave ifTrue: [ Smalltalk saveSession ]
]

{ #category : 'testing' }
ClapPharoApplication >> shouldQuit [
	^ ((self hasFlag: #keepAlive) or: [ self hasFlag: #'no-quit' ]) not
]

{ #category : 'testing' }
ClapPharoApplication >> shouldSave [
	^ self hasFlag: #save
]
