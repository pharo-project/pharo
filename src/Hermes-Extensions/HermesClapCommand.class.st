"
Class: HermesCommandLineHandler
                                                                                                    
This is the command line handler to load Hermes files in the image.
The format is intented to export any of the objects. However, the command line expects to have a package as its root element.
It loads the hermes files passed as parameter.
They are loaded in the order of the parameters.

Usage: loadHermes [--save] [extendedOptions] [<hermesFiles> ...]

Examples:
#Load the package Test-Package
pharo Pharo.image loadHermes Test-Package.hermes

#Load the package Test-Package and saving
pharo Pharo.image loadHermes --save Test-Package.hermes


Extensions
=========

no-fail-on-undeclared		It does not fail the loading if there are new undeclared variables. By default it fails.
on-duplication=action	When there is a duplication in the loading package what is the action to take:
fail		(default) Fail on a duplication
ignore	Ignore the error and do not do nothing. Keeping the image version
replace	Replace the image version with the one in the Hermes Package.

Examples

#Load the package Test-Package without failing on new undeclared variables.
pharo Pharo.image loadHermes --save --no-fail-on-undeclared Test-Package.hermes

#Load the package Test-Package replacing the image version with the ones in the hermes file.
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

	^ self
		  positional: #'on-duplication'
		  ifPresent: [ :action |
				  (#( fail ignore replace ) includes: action asSymbol)
					  ifTrue: [ action ]
					  ifFalse: [ #fail ] ]
		  ifAbsent: [ #fail ]
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
	SystemNotification signal: ('[Hermes] Reading ' , aFileName).

	reader := HEBinaryReader new
		stream: (File named: aFileName) readStream;
		yourself.

	readPackage := HEPackage readFrom: reader.

	SystemNotification signal: ('[Hermes] Installing ' , aFileName).
	HEExtendedInstaller new
		forOptions: self;
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

	self allHermesFiles ifEmpty: [ ^ self context exitFailure: 'Missing Hermes file as argument' ]
]
