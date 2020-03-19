Class {
	#name : #ClapMetacello,
	#superclass : #ClapPharoApplication,
	#category : #'Clap-Commands-Pharo'
}

{ #category : #'command line' }
ClapMetacello class >> metacello [
	<commandline>
	^ (ClapCommand id: #metacello)
		description: 'Load code using Metacello';
		add: ClapFlag forHelp;
		add: ClapCommand forHelp;
		add: ((ClapPositional id: #REPOSITORY)
			description: 'URL of the repository to load code from');
		add: ((ClapPositional id: #NAME)
			description: 'Name of the project to load');
		add: ((ClapFlag id: #groups)
			description: 'Names of groups to load, comma-separated';
			add: ((ClapPositional id: #GROUPS)
				meaning: [ :pos | pos word splitOn: $, ];
				implicitMeaning: [ #() ]));
		add: ((ClapFlag id: #version)
			description: 'Version to load (only valid for configurations)';
			add: (ClapPositional id: #VERSION));
		meaning: [ :args | 
			args	at: #helpFlag ifPresent: [ :help | help value; exitSuccess ].
			(self with: args) execute ]
]

{ #category : #baselines }
ClapMetacello >> baselineName [
	^ arguments at: #NAME
]

{ #category : #execution }
ClapMetacello >> execute [
	self outputStreamDo: [ :out | 
		out
			print: self repositoryUrl;
			print: self baselineName;
			newLine ]
]

{ #category : #accessing }
ClapMetacello >> groups [
	^ arguments at: #version
]

{ #category : #accessing }
ClapMetacello >> repositoryUrl [
	^ arguments at: #REPOSITORY
]

{ #category : #accessing }
ClapMetacello >> version [
	^ arguments at: #groups
]
