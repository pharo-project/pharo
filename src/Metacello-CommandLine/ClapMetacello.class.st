"
I allow to load code with Metacello from command line.
"
Class {
	#name : 'ClapMetacello',
	#superclass : 'ClapPharoApplication',
	#category : 'Metacello-CommandLine',
	#package : 'Metacello-CommandLine'
}

{ #category : 'command line' }
ClapMetacello class >> commandSpecification [

	<commandline>
	| spec |
	spec := (ClapCommandSpec id: #install)
		        description: 'Load and install code using Metacello';
		        commandClass: self;
		        addHelp;
		        add: ClapCommandSpec forHelp;
		        addPositional: #repository description: 'URL of the repository to load code from';
		        addPositional: #project description: 'Name of the project to load (i.e. the name coming after BaselineOf)';
		        addFlagWithPositional: #groups description: 'Names of groups to load, comma-separated';
		        addFlag: #strict
		        description:
			        'Perform a strict loading -- Avoid undeclared variables and syntax errors. Default: false, for compatibility';
		        addFlag: #signalErrorOnWarning description: 'Signal an error if a warning is raised during metacello load';
		        addFlag: #mvs
		        description:
			        'Choose the versions with minimal version selection: each project at the lowest version that meets every minimum asked for it';
		        addFlag: #verbose description: 'Report each warning of the resolution, not only how many there are of each kind';
		        add: ((ClapFlag id: #dev)
				         description:
					         'Load also the project NAME from REPOSITORY at its branch, tag or commit, with its comma-separated GROUPS or its default ones, instead of any version asked for it (repeatable): --dev NAME REPOSITORY [GROUPS]';
				         add: (ClapPositional id: #devProject);
				         add: (ClapPositional id: #devRepository);
				         add: ((ClapPositional id: #devGroups)
						          beOptional;
						          defaultValue: [ :match | '' ];
						          yourself);
				         multiple: true;
				         yourself);
		        yourself.

	self
		addNoSaveFlagTo: spec;
		addKeepAliveOptionTo: spec;
		addNoQuitOptionTo: spec;
		addRenameOptionTo: spec.

	^ (ClapCommandSpec id: #metacello)
		  addHelp;
		  add: ClapCommandSpec forHelp;
		  addSubcommand: spec;
		  yourself
]

{ #category : 'execution' }
ClapMetacello >> addProjectTo: aMetacello [
	"Name the project of the positionals, when they name one, as aMetacello's baseline"

	aMetacello
		baseline: (self projectNameIfAbsent: [ ^ self ]);
		repository: self repositoryUrl
]

{ #category : 'accessing' }
ClapMetacello >> devs [
	"Each --dev NAME REPOSITORY [GROUPS], in order: { name. repository. groups (empty for the default ones) }"

	^ self context lastSubcommand occurrencesOf: #dev collect: [ :flag |
			  {
				  (flag at: #devProject) value.
				  (flag at: #devRepository) value.
				  (($, split: (flag at: #devGroups) value) reject: [ :each | each isEmpty ]) asArray } ]
]

{ #category : 'execution' }
ClapMetacello >> execute [
	"Load the root the positionals name, if any, and the --dev roots"

	| metacello |
	self prepareImageAndExit.
	metacello := Metacello new
		             beStrict: self strict;
		             silently;
		             yourself.
	self addProjectTo: metacello.
	self usesMVS ifTrue: [ metacello mvs ].
	self devs do: [ :each | metacello dev: each first repository: each second loads: each third ].
	self shouldSignalErrorOnWarning ifTrue: [ metacello onWarning: [ :e | Error signal: e messageText in: e signalerContext ] ].
	self load: metacello.

	self saveSessionIfAsked
]

{ #category : 'accessing' }
ClapMetacello >> groups [

	^ self positional: #groups 
		ifPresent: [ :groups | $, split: groups ] 
		ifAbsent: [ #(  ) ]
]

{ #category : 'accessing' }
ClapMetacello >> isVerbose [

	^ self hasFlag: #verbose
]

{ #category : 'execution' }
ClapMetacello >> load: aMetacello [
	"Load aMetacello, then print the report of its resolution.
	Resolution errors are an outcome of the command, not a crash: the report lists them, and the command fails without a stack trace. They are caught here because an error left unhandled quits a headless image before any ensure: block runs"

	[ self groups ifEmpty: [ aMetacello load ] ifNotEmpty: [ aMetacello load: self groups ] ]
		on: MetacelloResolutionError
		do: [ :error |
			self printReportOf: error resolution.
			^ self exitFailure: 'Nothing was loaded: the resolution has errors (see the report above)' ].
	self printReportOf: aMetacello resolution
]

{ #category : 'execution' }
ClapMetacello >> metacelloScript [

	| metacello |
	metacello := Metacello new.
	self addProjectTo: metacello.
	self groups ifEmpty: [ metacello load ] ifNotEmpty: [ metacello load: self groups ]
]

{ #category : 'printing' }
ClapMetacello >> printReportOf: aResolution [

	self inform: (self isVerbose
			 ifTrue: [ aResolution verboseReport ]
			 ifFalse: [ aResolution report ])
]

{ #category : 'accessing' }
ClapMetacello >> projectNameIfAbsent: aBlock [
	"The project of the root the positionals name; aBlock's value when they name none (only --dev roots then)"

	^ self positional: #project ifAbsent: aBlock
]

{ #category : 'accessing' }
ClapMetacello >> repositoryUrl [
	"The repository of the root the positionals name"

	^ self positional: #repository
]

{ #category : 'testing' }
ClapMetacello >> shouldSave [
	^ (self hasFlag: #'no-save') not
]

{ #category : 'accessing' }
ClapMetacello >> shouldSignalErrorOnWarning [
	^ self hasFlag: #signalErrorOnWarning
]

{ #category : 'accessing' }
ClapMetacello >> strict [
	^ self hasFlag: #strict
]

{ #category : 'accessing' }
ClapMetacello >> usesMVS [

	^ self hasFlag: #mvs
]
