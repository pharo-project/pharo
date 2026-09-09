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
	#name : 'BasicHermesTool',
	#superclass : 'Object',
	#instVars : [
		'hermesFiles'
	],
	#category : 'Hermes-CommandLineHandler',
	#package : 'Hermes',
	#tag : 'CommandLineHandler'
}

{ #category : 'loading' }
BasicHermesTool class >> load: aListOfFileName [

	self new 
		fileNames: aListOfFileName;
		processFiles
]

{ #category : 'initialization' }
BasicHermesTool >> fileNames: aListOfFileName [

	hermesFiles := aListOfFileName select: [ :name | name endsWith: '.hermes' ]
]

{ #category : 'processing' }
BasicHermesTool >> processFile: aFileName [

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

{ #category : 'processing' }
BasicHermesTool >> processFiles [

	hermesFiles do: [ :name | self processFile: name ]
]
