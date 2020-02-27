"
This is the command line handler to load Hermes files in the image.
The format is intented to export any of the objects. However, the command line expects to have a package as its root element.
It loads the hermes files passed as parameter.
They are loaded in the order of the parameters.

Usage: loadHermes [--save] [extendedOptions] [<hermesFiles> ...]
	--save                		save after loading the packages if there is no error
	 <hermesFiles>      	a list of .hermes files to load in the image.
	extendedOptions 	read the extension section.
	
Examples:
	#Load the package Test-Package
	pharo Pharo.image loadHermes Test-Package.hermes
	
	#Load the package Test-Package and saving
	pharo Pharo.image loadHermes --save Test-Package.hermes
	
	
Extensions 
=========

When Hermes-Extensions is loaded additional options are present: 
	no-fail-on-undeclared		It does not fail the loading if there are new undeclared variables. By default it fails.
	on-duplication=action	When there is a duplication in the loading package what is the action to take:
								fail		(default) Fail on a duplication
								ignore	Ignore the error and do not do nothing. Keeping the image version
								replace	Replace the image version with the one in the Hermes Package.
									
Examples
								
	#Load the package Test-Package without failing on new undeclared variables.
	pharo Pharo.image loadHermes --save --no-fail-on-undeclared Test-Package.hermes

	#Load the package Test-Package replacing the image version with the ones in the hermes file.
	pharo Pharo.image loadHermes --save --on-duplication=replace Test-Package.hermes

"
Class {
	#name : #HermesCommandLineHandler,
	#superclass : #CommandLineHandler,
	#category : #'Hermes-CommandLineHandler'
}

{ #category : #accessing }
HermesCommandLineHandler class >> commandName [
	^ 'loadHermes'
]

{ #category : #accessing }
HermesCommandLineHandler class >> description [
	^ 'Loads the hermes files passed as parameter'
]

{ #category : #activation }
HermesCommandLineHandler >> activate [
	self activateHelp ifTrue: [ ^ self ].
	self validateParameters.
	
	self processFiles.
		
	(self hasOption: 'save')
		ifTrue: [ Smalltalk snapshot: true andQuit: false ].

	self exitSuccess
]

{ #category : #'processing files' }
HermesCommandLineHandler >> createInstaller [
	"In the basic installation, the bootstraped version of Hermes, 
	there is only one Installer, the HEInstaller. 
	When the extensions are installed the new installer to use is the HEExtendedInstaller."

	^ Smalltalk globals
		at: #HEExtendedInstaller
		ifPresent: [ :instClass | 
			instClass new
				forOptions: self commandLine;
				yourself ]
		ifAbsent: [ HEInstaller new ]
]

{ #category : #'processing files' }
HermesCommandLineHandler >> processFile: file [
	"It loads the package and installs it in the image"
	| installer reader readPackage |
	installer := self createInstaller.
	SystemNotification signal: ('[Hermes] Reading ' , file).

	reader := HEBinaryReader new
		stream: (File named: file) readStream;
		yourself.

	readPackage := HEPackage readFrom: reader.

	SystemNotification signal: ('[Hermes] Installing ' , file).
	installer installPackage: readPackage
]

{ #category : #'processing files' }
HermesCommandLineHandler >> processFiles [
	"The files are processed in order of the command line"
	(self commandLine allFilesWithExtension: '.hermes') do: [ :file | self processFile: file ]
]

{ #category : #validation }
HermesCommandLineHandler >> validateParameters [
	"The command line handle should have at least one .hermes file"
	(self commandLine hasFileWithExtension: '.hermes')
		ifFalse: [ ^ self exitFailure: 'Missing Hermes file as argument' ]
]
