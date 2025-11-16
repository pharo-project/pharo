"
I'm responsible to manage the default image options.

Usage: [<subcommand>] [--help] [--copyright] [--version] [--list] [ --no-quit ]
	--help       print this help message
	--copyright  print the copyrights
	--version    print the version for the image and the vm
	--list       list a description of all active command line handlers
	--no-quit    keep the image running without activating any other command line handler
	--deploymentPassword   if a password needs to be used by the user to launch the command
	--readWriteAccessMode, --readOnlyAccessMode, --writeOnlyAccessMode, --disabledAccessMode
	             specify disk access mode, read-write mode as default
	<subcommand> a valid subcommand in --list
	
Documentation:
A PharoCommandLineHandler handles default command line arguments and options.
The PharoCommandLineHandler is activated before all other handlers. 
It first checks if another handler is available. If so it will activate the found handler.
"
Class {
	#name : 'BasicPharoOptionsHandler',
	#superclass : 'Object',
	#instVars : [
		'arguments'
	],
	#classVars : [
		'CurrentOptionsHandler'
	],
	#category : 'System-CommandLineHandler',
	#package : 'System-CommandLineHandler'
}

{ #category : 'accessing' }
BasicPharoOptionsHandler class >> currentOptionHandler [

	^ CurrentOptionsHandler ifNil: [ CurrentOptionsHandler := self ]
]

{ #category : 'accessing' }
BasicPharoOptionsHandler class >> forArguments: arguments [

	^ self new
		arguments: arguments;
		yourself
]

{ #category : 'accessing' }
BasicPharoOptionsHandler class >> handle: arguments [

	^ (self forArguments: arguments) handlePharoArguments
]

{ #category : 'accessing' }
BasicPharoOptionsHandler >> arguments [
	^ arguments
]

{ #category : 'accessing' }
BasicPharoOptionsHandler >> arguments: aCollection [ 
	
	arguments := aCollection 
]

{ #category : 'accessing' }
BasicPharoOptionsHandler >> filteredArguments [
	
	arguments ifEmpty: [ ^ arguments ].
		
	arguments first = '--interactive'
		ifTrue: [ ^ arguments copyWithoutFirst ].
		
	^ arguments
]

{ #category : 'execution' }
BasicPharoOptionsHandler >> handlePharoArguments [

	^ self filteredArguments
]
