"
Usage: clean [ --release ]
	--release      do #cleanUpForRelease
	--production   do #cleanUpForProduction
	
Documentation:
	This allows to run the ImageCleaner from the commandLine.
	With no special option it runs Smalltalk cleanUp: true.

Usage:
	pharo Pharo.image clean
	pharo Pharo.image clean --release
"
Class {
	#name : 'ImageCleanerCliCommand',
	#superclass : 'ClapPharoApplication',
	#category : 'Tool-ImageCleaner',
	#package : 'Tool-ImageCleaner'
}

{ #category : 'command line' }
ImageCleanerCliCommand class >> commandSpecification [

	<commandline>
	^ (ClapCommandSpec id: #clean)
		  description: 'Cleans the image=, If no option specified, runs Smalltalk cleanUp: true.';
		  commandClass: self;
		  addHelp;
		  addFlag: #production description: 'Do production cleanup: do release cleanup + unload tests and example packages';
		  addFlag: #release description: 'Do release cleanup: runs Smalltalk cleanUp: true + garbage collect, clean undeclared, fix obsolete references, cleanup classes, rehashing, literals sharing, etc.';
		  yourself
]

{ #category : 'execution' }
ImageCleanerCliCommand >> execute [

	self runCleanup.
	Smalltalk saveSession
]

{ #category : 'testing' }
ImageCleanerCliCommand >> isProductionCleanup [

	^ self hasFlag: #production
]

{ #category : 'testing' }
ImageCleanerCliCommand >> isReleaseCleanup [

	^ self hasFlag: #release
]

{ #category : 'execution' }
ImageCleanerCliCommand >> runCleanup [

	self isReleaseCleanup ifTrue: [ ^ ImageCleaner cleanUpForRelease ].

	self isProductionCleanup ifTrue: [ ^ ImageCleaner cleanUpForProduction ].

	Smalltalk cleanUp: true except: #( ) confirming: false
]
