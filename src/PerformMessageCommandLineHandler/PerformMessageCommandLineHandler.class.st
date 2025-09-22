"
Usage: perform <global> <messageSelector>  [ <arguments> ] [ --save ]
	global	            A class name or a global name. Receiver of the message (e.g. Smalltalk)
	messageSelector   The message selector to be perfomed
	
Documentation:
Performs a message on a given class or global object. This command-line handler can be used to execute some code if a compiler is not loaded in the image. All message arguments are strings.

Examples:
	pharo Pharo.image perform Smalltalk garbageCollect --save
"
Class {
	#name : 'PerformMessageCommandLineHandler',
	#superclass : 'CommandLineHandler',
	#instVars : [
		'targetClass',
		'messageArguments',
		'message'
	],
	#category : 'PerformMessageCommandLineHandler',
	#package : 'PerformMessageCommandLineHandler'
}

{ #category : 'accessing' }
PerformMessageCommandLineHandler class >> commandName [
	^ 'perform'
]

{ #category : 'activation' }
PerformMessageCommandLineHandler >> activate [

	self shouldActivateHelp ifTrue: [
			self printHelp.
			self exitSuccess ].

	self execute.

	self shouldSave ifTrue: [ Smalltalk saveSession ].
	self exitSuccess
]

{ #category : 'actions' }
PerformMessageCommandLineHandler >> execute [
	| result |

	self parseArguments.

	result := targetClass perform: message withArguments: messageArguments.
	
	self stdout
		print: result;
		lf
]

{ #category : 'accessing' }
PerformMessageCommandLineHandler >> message [

	^ message
]

{ #category : 'accessing' }
PerformMessageCommandLineHandler >> messageArguments [

	^ messageArguments
]

{ #category : 'parsing' }
PerformMessageCommandLineHandler >> parseArguments [
	| arguments |

	arguments := self arguments reject: [ :argument | argument beginsWith: '-']. "ignore options"

	(arguments size < 2)
		ifTrue: [ ^ self exitFailure: 'Not enough arguments' ].

	targetClass := Smalltalk globals at: arguments first asSymbol ifAbsent: [
		  ^ self exitFailure: 'Unknown class' ].
	
	message := arguments second asSymbol.
	messageArguments := (arguments allButFirst: 2) asArray
]

{ #category : 'actions' }
PerformMessageCommandLineHandler >> printHelp [
	
	self stderr
		nextPutAll: self class comment; lf
]

{ #category : 'testing' }
PerformMessageCommandLineHandler >> shouldActivateHelp [
	
	^ self arguments includes: ('--help')
]

{ #category : 'testing' }
PerformMessageCommandLineHandler >> shouldSave [
	
	^ self arguments includes: ('--save')
]

{ #category : 'accessing' }
PerformMessageCommandLineHandler >> targetClass [
	^ targetClass
]
