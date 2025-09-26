"
This is the command line handler to load Hermes files in the image during the bootstrap.
Another command will be loaded after the bootstrap with more options.
The format is intented to export any of the objects. However, the command line expects to have a package as its root element.
It loads the hermes files passed as parameter.
They are loaded in the order of the parameters.

Usage: loadHermes [--save] [<hermesFiles> ...]
--save                		save after loading the packages if there is no error

"
Class {
	#name : 'BasicHermesCommandLineHandler',
	#superclass : 'CommandLineHandler',
	#category : 'Hermes-CommandLineHandler',
	#package : 'Hermes',
	#tag : 'CommandLineHandler'
}

{ #category : 'accessing' }
BasicHermesCommandLineHandler class >> commandName [
	^ 'loadHermes'
]

{ #category : 'handler selection' }
BasicHermesCommandLineHandler class >> isResponsibleFor: aCommandLineArguments [

	aCommandLineArguments arguments ifEmpty: [ ^ false ].
	
	"If HermesClapCommand is available, do not use this basic command line handler"
	^ (aCommandLineArguments arguments first = self commandName) 
		and: [ (Smalltalk globals hasClassNamed: #HermesClapCommand) not ]
]

{ #category : 'activation' }
BasicHermesCommandLineHandler >> activate [

	self allHermesFiles do: [ :name | self processFile: name ].
	Smalltalk saveSession.
	self exitSuccess
]

{ #category : 'accessing' }
BasicHermesCommandLineHandler >> allHermesFiles [

	^ self arguments
		select: [ :arg | arg endsWith: '.hermes' ]
]

{ #category : 'execution' }
BasicHermesCommandLineHandler >> processFile: aFileName [

	"It loads the package and installs it in the image"
	| reader readPackage |
	SystemNotification signal: ('[Basic Hermes] Reading ' , aFileName).

	reader := HEBinaryReader new
		stream: (File named: aFileName) readStream;
		yourself.

	readPackage := HEPackage readFrom: reader.

	SystemNotification signal: ('[Basic Hermes] Installing ' , aFileName).
	HEInstaller new installPackage: readPackage
]
