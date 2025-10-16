"
I'm responsible of managing arguments given to the image.
I then run the appropriate actions if any.
I provide core functionalities to implement a command line handler that can replace the current one.
"
Class {
	#name : 'CommandLineHandler',
	#superclass : 'Object',
	#instVars : [
		'commandLine',
		'session',
		'stdout',
		'stderr'
	],
	#category : 'System-BasicCommandLineHandler-Base',
	#package : 'System-BasicCommandLineHandler',
	#tag : 'Base'
}

{ #category : 'instance creation' }
CommandLineHandler class >> commandLine: aCommandLine [

	^ self new
		commandLine: aCommandLine;
		yourself
]

{ #category : 'registration' }
CommandLineHandler class >> registerAsCommandLinehandler [

	SessionManager default in: [ :manager |
		manager
			register: (NamedSessionHandler forClassNamed: self name id: #commandLineHandler)
			inCategory: manager systemCategory ]
]

{ #category : 'system startup' }
CommandLineHandler class >> startUp: isImageStarting [
	"only handle when launching a new image"
	isImageStarting ifFalse: [ ^ self ].

	Smalltalk session
		addDeferredStartupAction: [ self new activate ]
]

{ #category : 'activation' }
CommandLineHandler >> activate [

	"Do not redefine this method in subclasses. Use #activateCommand for command specific code"
	self parseGeneralOptions.
	
	[ self activateAndHandleExit ]
		forkAt: Processor userSchedulingPriority
		named: 'Commandline handler process'
]

{ #category : 'activation' }
CommandLineHandler >> activateAndHandleExit [

	[ self activateCommand ]
	on: Exit do: [ :exit | self handleExit: exit ]
	
]

{ #category : 'activation' }
CommandLineHandler >> activateCommand [

	self subclassResponsibility 
]

{ #category : 'accessing' }
CommandLineHandler >> commandLine [ 

	^ commandLine ifNil: [ commandLine := CommandLineArguments new ]
]

{ #category : 'accessing' }
CommandLineHandler >> commandLine: aCommandLineArguments [

	commandLine := aCommandLineArguments
]

{ #category : 'exiting' }
CommandLineHandler >> exitFailure: aMessage [
	self hasSessionChanged
		ifTrue: [ ^ self ].
	Exit signalFailure: aMessage
]

{ #category : 'exiting' }
CommandLineHandler >> exitSuccess [
	self hasSessionChanged
		ifTrue: [ ^ self ].
	Exit signalSuccess
]

{ #category : 'activation' }
CommandLineHandler >> handleExit: exit [

	Smalltalk isInteractive ifFalse: [ ^ exit pass ].
	
	"We are in interactive mode"
	exit isSuccess
		ifFalse: [ ^ Error signal: exit messageText ].
		
	self inform: 'Command line successfully finished'.
]

{ #category : 'testing' }
CommandLineHandler >> hasSessionChanged [
	"check whether the session has changed since the commandline handler as been created"

	^ session ~~ Smalltalk session
]

{ #category : 'initialization' }
CommandLineHandler >> initialize [
	super initialize.
	session := Smalltalk session.
]

{ #category : 'testing' }
CommandLineHandler >> isInteractiveAndNoArguments [
	"WARNING: should be used after general options are parsed"

	^ Smalltalk isInteractive and: [ self commandLine hasArguments not ]
]

{ #category : 'parsing' }
CommandLineHandler >> parseGeneralOptions [

	| filteredArguments |
	filteredArguments := PharoOptionsHandler handle: self commandLine arguments.
	self commandLine arguments: filteredArguments
]

{ #category : 'accessing' }
CommandLineHandler >> stderr [

	"install the line end conversion and initialize the converter"
	^ stderr ifNil: [ stderr := VTermOutputDriver stderr ]
]

{ #category : 'accessing' }
CommandLineHandler >> stdout [ 

	"install the line end conversion and initialize the converter"
	^ stdout ifNil: [ stdout := VTermOutputDriver stdout ]
]
