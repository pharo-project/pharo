"
I am the command line command loading Hermes files in the image, in the order they are given. Each file is expected to contain a package.

I install the packages with a HEExtendedInstaller, whose options I set from the command line. I am only available when Hermes-Extensions is loaded: without it, BasicHermesTool loads Hermes files with the minimal installer and no option, as during the bootstrap of Pharo.

Usage: loadHermes [--save] [--no-fail-on-undeclared] [--on-duplication <action>] <hermesFiles>

	--save	saves the image after loading the files
	--no-fail-on-undeclared	does not fail when the loading creates new undeclared variables
	--on-duplication <action>	what to do when an element of a package already exists in the image: fail (default), ignore to keep the version of the image, or replace to use the version of the file

Examples:

	pharo Pharo.image loadHermes --save Test-Package.hermes
	pharo Pharo.image loadHermes --save --on-duplication replace Test-Package.hermes
"
Class {
	#name : 'HermesClapCommand',
	#superclass : 'ClapPharoApplication',
	#category : 'Hermes-Extensions-Command',
	#package : 'Hermes-Extensions',
	#tag : 'Command'
}

{ #category : 'command line' }
HermesClapCommand class >> commandSpecification [

	<commandline>
	| spec |
	spec := (ClapCommandSpec id: #loadHermes)
		        description: 'Loads the hermes files passed as parameter';
		        commandClass: self;
		        addHelp;
		        addPositional: #FILE spec: [ :positional |
					positional
						description: 'The Hermes file to load';
						multiple: true ];
		        addFlag: #'no-fail-on-undeclared' description: 'Does not fail the loading if there are new undeclared variables.';
		        addFlagWithPositional: #'on-duplication' description: 'When there is a duplication in the loading package what is the action to take:
- fail			(default) Fail on a duplication
- ignore		Ignore the error and do not do nothing. Keeping the image version
- replace		Replace the image version with the one in the Hermes Package.';
		        yourself.

	self addSaveFlagTo: spec.
	^ spec
]

{ #category : 'accessing' }
HermesClapCommand >> allHermesFiles [

	^ (self positional: #FILE ifAbsent: [ #() ]) 
		select: [ :arg | arg endsWith: '.hermes' ]
]

{ #category : 'accessing' }
HermesClapCommand >> duplicationMode [

	^ self positional: #'on-duplication' ifAbsent: [ #fail ]
]

{ #category : 'execution' }
HermesClapCommand >> execute [ 

	self prepareImageAndExit.
	
	self validateParameters.
	self processFiles.
	self saveSessionIfAsked
]

{ #category : 'execution' }
HermesClapCommand >> processFile: aFileName [
	"It loads the package and installs it in the image"

	| reader readPackage |
	SystemNotification signal: '[Hermes] Reading ' , aFileName.

	reader := HEBinaryReader new
		          stream: (File named: aFileName) readStream;
		          yourself.

	readPackage := HEPackage readFrom: reader.

	SystemNotification signal: '[Hermes] Installing ' , aFileName.
	HEExtendedInstaller new
		failOnUndeclared: self shouldFailOnUndeclared;
		duplicationMode: (HEDuplicationModeStrategy forOption: self duplicationMode);
		installPackage: readPackage
]

{ #category : 'execution' }
HermesClapCommand >> processFiles [

	self allHermesFiles do: [ :name | self processFile: name ]
]

{ #category : 'testing' }
HermesClapCommand >> shouldFailOnUndeclared [

	^ (self hasFlag: #'no-fail-on-undeclared') not
]

{ #category : 'validating' }
HermesClapCommand >> validateParameters [

	self allHermesFiles ifEmpty: [ ^ self context exitFailure: 'Missing Hermes file as argument' ].
	((HEDuplicationModeStrategy allSubclasses collect: [ :strategy | strategy keyword ]) includes: self duplicationMode asString) ifFalse: [
		^ self context exitFailure: 'Unknown action on duplication: ' , self duplicationMode ]
]
