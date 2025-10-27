"
Performs a message on a given class or global object. This command can be used to execute some code if a compiler is not loaded in the image. All message arguments are strings.
Example: pharo Pharo.image perform Smalltalk garbageCollect --save
   
Usage: perform [--help] [--save] [--no-quit] [--as-array <array-argument>] [<global>] [<messageSelector>] [<arguments>]
   
Parameters:
     <global>    A class name or a global name. Receiver of the message (e.g. Smalltalk)
     <messageSelector>
                     The message selector to be perfomed
     <arguments>
                     arguments to be passes to the message selector. All arguments after the special argument -- will be wrapped into an array and passed as a single argument.
   
Options:
     --help      Prints this documentation
     --save      Save the image after the command terminates
     --no-quit   [deprecated] keep the image running without activating any other command line handler
     --as-array <array-argument>
                     Following arguments will be wrapped into an array and passed as a single argument. Should be used after <arguments>
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
	#category : 'System-BasicCommandLineHandler-Base',
	#package : 'System-BasicCommandLineHandler',
	#tag : 'Base'
}

{ #category : 'accessing' }
PerformMessageCommandLineHandler class >> commandName [
	^ 'perform'
]

{ #category : 'class initialization' }
PerformMessageCommandLineHandler class >> initialize [
	
	self registerAsCommandLinehandler
]

{ #category : 'activation' }
PerformMessageCommandLineHandler >> activateCommand [
	
	self isInteractiveAndNoArguments ifTrue: [ ^ self ].
	
	self isPerformCommand 
		ifTrue: [ self commandLine removeFirst ]
		ifFalse: [ self printHelp.
					self exitFailure: 'perform keyword missing!' ].
	
	self shouldActivateHelp 
		ifTrue: [ self printHelp.
					self exitSuccess ].

	self execute.

	self shouldSave ifTrue: [ Smalltalk saveSession ].
	self shouldQuit ifTrue: [ self exitSuccess ]
]

{ #category : 'accessing' }
PerformMessageCommandLineHandler >> arguments [ 

	^ self commandLine arguments
]

{ #category : 'accessing' }
PerformMessageCommandLineHandler >> arrayArgumentMarker [

	^ '--as-array'
]

{ #category : 'accessing' }
PerformMessageCommandLineHandler >> commandOptions [

	^ commandOptions ifNil: [ #() ]
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

{ #category : 'testing' }
PerformMessageCommandLineHandler >> isPerformCommand [

	self commandLine withFirstArgument: [ :arg |
			^ arg = self class commandName ].

	^ false
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
	
	| str targetClassName |
	commandOptions := OrderedCollection new.
	str := self arguments readStream.
	[ str atEnd not and: [ str peek beginsWith: '--' ] ] whileTrue: [ commandOptions add: str next ].

	(self arguments size - commandOptions size < 2)
		ifTrue: [ ^ self exitFailure: 'Not enough arguments' ].

	targetClassName := str next.
	targetClass :=  self class environment
		at: targetClassName asSymbol
		ifAbsent: [ ^ self exitFailure: 'Unknown class ', targetClassName ].
	message := str next asSymbol.
	
	self parseMessageArguments: str upToEnd
]

{ #category : 'parsing' }
PerformMessageCommandLineHandler >> parseMessageArguments: remainingArguments [
	| splittedArguments |

	"All options passed to the perform command after the -- argument are considered as arguments to the message to be called.
	It is useful, for example, to pass flags to the called message."

	splittedArguments := { self arrayArgumentMarker } split: remainingArguments.
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
	
	self commandLine withFirstArgument: [ :first | ^ first = '--help' ].
	^ false 
	
]

{ #category : 'testing' }
PerformMessageCommandLineHandler >> shouldQuit [
	
	Smalltalk isInteractive ifTrue: [ ^ false ].
	
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
