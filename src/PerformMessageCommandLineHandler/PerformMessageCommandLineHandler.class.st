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
		'message',
		'messageArguments',
		'commandOptions'
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
	self shouldQuit ifTrue: [ self exitSuccess ]
]

{ #category : 'accessing' }
PerformMessageCommandLineHandler >> commandOptions [

	^ commandOptions ifNil: [ #() ]
]

{ #category : 'accessing' }
PerformMessageCommandLineHandler >> commandOptionsEndMarker [

	^ '--'
]

{ #category : 'actions' }
PerformMessageCommandLineHandler >> execute [
	| result |

	self parseArguments.

	result := self targetClass perform: self message withArguments: self messageArguments.
	
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

	^ messageArguments asArray
]

{ #category : 'parsing' }
PerformMessageCommandLineHandler >> parseArguments [
	
	| str |
	commandOptions := OrderedCollection new.
	str := self arguments readStream.
	[ str atEnd not and: [ str peek beginsWith: '--' ] ] whileTrue: [ commandOptions add: str next ].

	targetClass :=  Smalltalk globals 
		at: str next asSymbol
		ifAbsent: [ ^ self exitFailure: 'Unknown class' ].
	message := str next asSymbol.
	
	self parseMessageArguments: str upToEnd
]

{ #category : 'parsing' }
PerformMessageCommandLineHandler >> parseMessageArguments: remainingArguments [
	| splittedArguments |

	"All options passed to the perform command after the -- argument are considered as arguments to the message to be called.
	It is useful, for example, to pass flags to the called message."

	splittedArguments := { self commandOptionsEndMarker } split: remainingArguments.
	messageArguments := (splittedArguments size = 1) 
		ifTrue: [ splittedArguments first ]
		ifFalse: [ splittedArguments first copyWith: splittedArguments second ]

	
]

{ #category : 'actions' }
PerformMessageCommandLineHandler >> printHelp [
	
	self stderr
		nextPutAll: self class comment; lf
]

{ #category : 'testing' }
PerformMessageCommandLineHandler >> shouldActivateHelp [
	
	^ self arguments first = '--help'
]

{ #category : 'testing' }
PerformMessageCommandLineHandler >> shouldQuit [
	
	^ (self commandOptions includes: ('--no-quit')) not
]

{ #category : 'testing' }
PerformMessageCommandLineHandler >> shouldSave [
	
	^ self commandOptions includes: ('--save')
]

{ #category : 'accessing' }
PerformMessageCommandLineHandler >> targetClass [
	^ targetClass
]
