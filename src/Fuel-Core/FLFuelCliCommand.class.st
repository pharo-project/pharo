"
This command will load the <fuelFile> and materialize/install it's contents. If no argument is specified the image exits.

Example:

- Load a fuel file and save and quit the image with the contents:
pharo Pharo.image --save path/to/foo.fuel

- Load the contents of foo.fuel and save the image, but continue running:
pharo Pharo.image --save --keepAlive path/to/foo.fuel

- Load the contents of foo.fuel and continue running without saving:
pharo Pharo.image --keepAlive path/to/foo.fuel
"
Class {
	#name : 'FLFuelCliCommand',
	#superclass : 'ClapPharoApplication',
	#category : 'Fuel-Core-Command',
	#package : 'Fuel-Core',
	#tag : 'Command'
}

{ #category : 'command line' }
FLFuelCliCommand class >> commandSpecification [

	<commandline>
	| spec |
	spec := (ClapCommandSpec id: #fuel)
		        description: 'I load .fuel files that are passed as arguments. I materialize/install it''s contents';
		        commandClass: self;
		        addHelp;
		        addPositional: #FILE spec: [ :positional |
					positional
						description: 'The fuel file to load';
						multiple: true ];
		        yourself.

	self 
		addSaveFlagTo: spec;
		addKeepAliveOptionTo: spec.
	^ spec
]

{ #category : 'accessing' }
FLFuelCliCommand >> allFuelFiles [

	^ (self positional: #FILE ifAbsent: [ #() ]) 
		select: [ :arg | arg endsWith: '.fuel' ]
]

{ #category : 'execution' }
FLFuelCliCommand >> execute [ 

	super execute.
	
	self validateParameters.
	self processFiles.
	
	self saveSessionIfAsked

]

{ #category : 'execution' }
FLFuelCliCommand >> processFile: aFileReference [

	self context stdout
		nextPutAll: 'Materializing from file: ';
		nextPutAll: aFileReference fullName;
		lf.
	FLMaterializer new
		filePath: aFileReference pathString;
		materializeRoot.
	self context stdout
		nextPutAll: 'Materialization finished';
		lf
]

{ #category : 'execution' }
FLFuelCliCommand >> processFiles [

	self allFuelFiles do: [ :name |
		self processFile: (FileSystem disk resolve: name) asFileReference ]
]

{ #category : 'validating' }
FLFuelCliCommand >> validateParameters [

	self allFuelFiles ifEmpty: [ ^ self context exitFailure: 'Missing Fuel file as argument' ]
]
