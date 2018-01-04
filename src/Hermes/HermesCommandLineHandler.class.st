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
	^ 'Loads the hermes file passed as parameter'
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
	| installer reader readPackage |
	installer := self createInstaller.
	('[Hermes] Reading ' , file) logCr.

	reader := HEBinaryReader new
		stream: (File named: file) readStream;
		yourself.

	readPackage := HEPackage readFrom: reader.

	('[Hermes] Installing ' , file) logCr.
	installer installPackage: readPackage
]

{ #category : #'processing files' }
HermesCommandLineHandler >> processFiles [
	(self commandLine allFilesWithExtension: '.hermes') do: [ :file | self processFile: file ]
]

{ #category : #validation }
HermesCommandLineHandler >> validateParameters [
	(self commandLine hasFileWithExtension: '.hermes')
		ifFalse: [ ^ self exitFailure: 'Missing Hermes file as argument' ]
]
