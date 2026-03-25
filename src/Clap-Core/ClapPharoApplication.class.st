"
I am an abstract class providing some support to Pharo commands available from command-line.
"
Class {
	#name : 'ClapPharoApplication',
	#superclass : 'ClapApplication',
	#category : 'Clap-Core-Pharo-Integration',
	#package : 'Clap-Core',
	#tag : 'Pharo-Integration'
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

{ #category : 'command line' }
ClapPharoApplication class >> commandSpecification [
	^ (ClapRoot id: #clap)
		description: 'Pharo command line entry point.';
		commandClass: self;
		canonicalName: '<subcommand>';
		addHelp;
		add: ClapCommandSpec forHelp;
		addFlag: #'no-default-preferences' description: 'do not load any preferences from the default locations';
		addFlag: #'preferences-file' withPositional: #'FILE' description: 'load the preferences from the given <FILE>';
		addFlag: #version description: 'print the version for the image and the vm';

		" for now, flags are defined in subcommands
		addFlag: #save description: 'Save the image after the command terminates';
		addFlag: #noSave description: 'Do not save the image even if the command is stateful';
		addFlag: #rename 
			description: 'Rename the image as <NAME> before running the command' 
			positionalSpec: [ :spec | spec identifier: #NAME];
		addFlag: #keepAlive description: 'Keep the image running after the command terminates';"
		yourself
]

{ #category : 'execution' }
ClapPharoApplication >> doNotQuitImage [ 

	self context doNotObeyExits
]

{ #category : 'execution' }
ClapPharoApplication >> execute [

	(self hasFlag: #version) 
		ifTrue: [ self runPharoVersionCommand ]
]

{ #category : 'private - printing' }
ClapPharoApplication >> inform: aString [

	self outputStreamDo: [ :out | out << aString << OSPlatform current lineEnding ]
]

{ #category : 'private' }
ClapPharoApplication >> pharoVersionContext [
	
	"To avoid a hard dependency on ClapPharoVersion, we instead re-run the matching with arguments 
	that should end in the ClapPharoVersion subcommand"
	
	^ (ClapContext specification: context specification)
		arguments: #('printVersion' '--full');
		setStdio: context stdio;
		match;
		validateAll;
		yourself
]

{ #category : 'execution' }
ClapPharoApplication >> prepareImage [

	self positional: #rename ifPresent: [ :rename |
			self flag: 'check if it already exists & not same as current'.
			Smalltalk changeImagePathTo: rename value asFileReference ]
]

{ #category : 'execution' }
ClapPharoApplication >> prepareImageAndExit [

	self prepareImage.
	self shouldQuit ifFalse: [ self doNotQuitImage ]
]

{ #category : 'execution' }
ClapPharoApplication >> runPharoVersionCommand [
	
	self pharoVersionContext command executeOrPrintHelp
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
