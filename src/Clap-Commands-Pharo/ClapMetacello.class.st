"
I allow to load code with Metacello from command line.
"
Class {
	#name : 'ClapMetacello',
	#superclass : 'ClapPharoApplication',
	#category : 'Clap-Commands-Pharo',
	#package : 'Clap-Commands-Pharo'
}

{ #category : 'command line' }
ClapMetacello class >> metacello [
	<commandline>
	^ (ClapCommandSpec id: #metacello)
		description: 'Load code using Metacello';
		commandClass: self;
		addHelp;
		add: ClapCommandSpec forHelp;
		addPositional: #REPOSITORY description: 'URL of the repository to load code from';
		addPositional: #NAME description: 'Name of the project to load';
		addFlagWithPositional: #groups description: 'Names of groups to load, comma-separated';
		addFlagWithPositional: #version description: 'Version to load (only valid for configurations)';
		yourself
]

{ #category : 'baselines' }
ClapMetacello >> baselineName [
	^ self positional: #NAME
]

{ #category : 'execution' }
ClapMetacello >> execute [
	self outputStreamDo: [ :out | 
		out
			print: self repositoryUrl;
			print: self baselineName;
			newLine ]
]

{ #category : 'accessing' }
ClapMetacello >> groups [
	^ $, split: (self positional: #version)
]

{ #category : 'accessing' }
ClapMetacello >> repositoryUrl [
	^ self positional: #REPOSITORY
]

{ #category : 'accessing' }
ClapMetacello >> version [
	^ self positional: #groups
]
