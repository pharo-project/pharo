"
Usage: clean [ --release ]
	--release        do #cleanUpForRelease
	--production   do #cleanUpForProduction
	
Documentation:
	This allows to run the ImageCleaner from the commandLine.
	With no special option it runs Smalltalk cleanUp: true.

Usage:
	pharo Pharo.image clean
	pharo Pharo.image clean --release
"
Class {
	#name : #ImageCleanerCommandLineHandler,
	#superclass : #CommandLineHandler,
	#category : #Tool-ImageCleaner
}

{ #category : #accessing }
ImageCleanerCommandLineHandler class >> commandName [
	^ 'clean'
]

{ #category : #accessing }
ImageCleanerCommandLineHandler class >> description [
	^ 'Run image cleanup'
]

{ #category : #activation }
ImageCleanerCommandLineHandler >> activate [
	self activateHelp
		ifTrue: [  ^ self ].
	self cleanUpImage.
	Smalltalk snapshot: true andQuit: true.
]

{ #category : #actions }
ImageCleanerCommandLineHandler >> cleanUpImage [
	(self hasOption: 'release')
		ifTrue: [^ImageCleaner cleanUpForRelease].
	(self hasOption: 'production')	
		ifTrue: [ ^ImageCleaner cleanUpForProduction].
	Smalltalk cleanUp: true except: #() confirming: false.	
]
