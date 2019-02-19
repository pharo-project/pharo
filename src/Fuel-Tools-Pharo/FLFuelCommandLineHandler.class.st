"
I handle .fuel files that are passed as arguments when starting the image. 

Usage: fuel [save] [quit] <fuelFile>
 
	save    save the image after loading <fuelFile>
	quit    Don't save the image and directly quit the image fater loading <fuelFile>
	
Documentation:
This command will load the <fuelFile> and materialize/install it's contents. If no argument is specified the image continues running with the loaded contents.


Example:

	#Load a fuel file and save and quit the image with the contents:
	pharo Pharo.image save quit path/to/foo.fuel

	#Load the contents of foo.fuel and save the image, but continue running:
	pharo Pharo.image save path/to/foo.fuel
	
	#Load the contents of foo.fuel and continue running without saving:
	pharo Pharo.image path/to/foo.fuel

"
Class {
	#name : #FLFuelCommandLineHandler,
	#superclass : #CommandLineHandler,
	#instVars : [
		'files'
	],
	#category : #'Fuel-Tools-Pharo-CommandLineHandler'
}

{ #category : #accessing }
FLFuelCommandLineHandler class >> commandName [
	^ 'Fuel'
]

{ #category : #accessing }
FLFuelCommandLineHandler class >> description [
	^ 'Loads fuel files'
]

{ #category : #accessing }
FLFuelCommandLineHandler class >> fileExtension [
	^ '.fuel'
]

{ #category : #'handler selection' }
FLFuelCommandLineHandler class >> isResponsibleFor: aCommandLine [
	"This handler is reponsible only for .fuel files"
	(aCommandLine hasFileWithExtension: self fileExtension)
		ifTrue: [ ^ true ].
	^ super isResponsibleFor: aCommandLine
	
]

{ #category : #activation }
FLFuelCommandLineHandler >> activate [
	self manageFiles.
	(self commandLine hasOption: 'save') ifTrue: [
		ThreadSafeTranscript install.
		SessionManager default addDeferredStartupAction: [
			Smalltalk snapshot: true andQuit: true.
			NonInteractiveTranscript new install ] ].
	(self commandLine hasOption: 'quit') ifTrue: [
		self exitSuccess ]
]

{ #category : #accessing }
FLFuelCommandLineHandler >> fileExtension [
	^ self class fileExtension
]

{ #category : #manage }
FLFuelCommandLineHandler >> manageFile: aFileReference [
	Stdio stdout
		nextPutAll: 'Materializing from file: ';
		nextPutAll: aFileReference fullName;
		lf.
	aFileReference readStreamDo: [ :stream |
		stream binary.
		FLMaterializer new materializeFrom: stream ].
	Stdio stdout
		nextPutAll: 'Materialization finished';
		lf
]

{ #category : #manage }
FLFuelCommandLineHandler >> manageFiles [
	"Load all the source files in the given array."
	files := (self commandLine allFilesWithExtension: self fileExtension)
		 collect: [ :each | (FileSystem disk resolve: each) asFileReference ].
	files ifNil: [ ^self ].
	files do: [ :reference | self manageFile: reference ] 

]
