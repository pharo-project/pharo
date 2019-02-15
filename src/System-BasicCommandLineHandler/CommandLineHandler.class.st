"
A CommandLineHandler is activated by the CommandLine. 

The responsible handler with the highest priority is selected and its instance-side method #activate is invoked.

By default the handlers are selected by their class name. In the following shell invocation the FooHandler is chosen:

	pharo Pharo.image FooHandler

A handler may provide a short name with the class-side #commandName method. If the FooHandler defined #commandName returning 'foo' it would be activated with the following shell invocation:
	
	pharo Pharo.image foo
	
For more sophisticated handler selection the CommandLineHandler should implement the #isResponsibleFor: class-side method. An instance of the current command line options is passed to this method which should then return a boolean.

Between all the responsible handlers the one with the highes #priority is chosen. To change the priority overwrite the class-side accessor.

"
Class {
	#name : #CommandLineHandler,
	#superclass : #Object,
	#instVars : [
		'commandLine',
		'session',
		'stdout',
		'stderr'
	],
	#category : #'System-BasicCommandLineHandler-Base'
}

{ #category : #'instance creation' }
CommandLineHandler class >> activateWith: aCommandLine [ 
	
	^ self new 
		commandLine: (self prepareSubcommand: aCommandLine);
		activate
]

{ #category : #accessing }
CommandLineHandler class >> allHandlers [
	^ self allSubclasses reject: [ :handler| handler isAbstract ]
]

{ #category : #'instance creation' }
CommandLineHandler class >> commandLine: aCommandLine [ 
	
	^ self new 
		commandLine: aCommandLine;
		yourself
]

{ #category : #accessing }
CommandLineHandler class >> commandName [
	" Overwrite this method to provide a short name for this command handler.
	You may pass the commandName as first argument to the image to select this handler"
	^ self name
]

{ #category : #accessing }
CommandLineHandler class >> description [
	"This method should return a short one-line description of the command"
	^ nil
]

{ #category : #accessing }
CommandLineHandler class >> isAbstract [
	^ self = CommandLineHandler
]

{ #category : #'handler selection' }
CommandLineHandler class >> isResponsibleFor: aCommandLineArguments [
	^ aCommandLineArguments includesSubCommand: self commandName
]

{ #category : #private }
CommandLineHandler class >> prepareSubcommand: commandLineArguments [

	"strip the subcommand name from the arguments"
	commandLineArguments withFirstArgument: [ :arg|
		arg = self commandName 
			ifTrue: [ ^ commandLineArguments copySubcommand ]].
	
	"not a subcommand hence we keep the same args"
	^ commandLineArguments
]

{ #category : #'handler selection' }
CommandLineHandler class >> priority [
	^ 0
]

{ #category : #testing }
CommandLineHandler class >> requireDeploymentPassword [
	"Here so that we can distinguish user application services command line handlers from usual pharo command line handlers.
	The objective is to be able to filter command line accessible by the user.
	Command line handlers that are not application services can be protected by a password in deployment."

	^ true
]

{ #category : #'handler selection' }
CommandLineHandler class >> selectHandlersFor: aCommandLine [

	^ self allHandlers select: [ :handlerClass|
		handlerClass isResponsibleFor: aCommandLine ]
]

{ #category : #utilities }
CommandLineHandler >> << aString [
	^ self stdout 
		nextPutAll: aString;
		yourself
]

{ #category : #activation }
CommandLineHandler >> activate [
	 self subclassResponsibility
]

{ #category : #activation }
CommandLineHandler >> activateHelp [
	"Default help implementation, running #help if the only argument is --help "
	((self hasOption: 'help') and: [ self arguments size = 1 ])
		ifTrue: [ 
			self help.
			^ true ].
	^ false
]

{ #category : #activation }
CommandLineHandler >> activateHelpWithoutArguments [
	"Default help implementation, running #help if the there is no argument or a single one which is --help "
	((self hasOption: 'help') or: [ self arguments isEmpty ])
		ifTrue: [ 
			self help.
			^ true ].
	^ false
]

{ #category : #accessing }
CommandLineHandler >> allHandlers [ 
	^ CommandLineHandler allHandlers 
]

{ #category : #'accessing arguments' }
CommandLineHandler >> argumentAt: anInteger [
	^ self commandLine argumentAt: anInteger
]

{ #category : #'accessing arguments' }
CommandLineHandler >> arguments [
	^ self commandLine arguments
]

{ #category : #accessing }
CommandLineHandler >> commandLine [
	^ commandLine
]

{ #category : #accessing }
CommandLineHandler >> commandLine: aCommandLine [ 
	commandLine := aCommandLine
]

{ #category : #accessing }
CommandLineHandler >> commandName [
	^ self class commandName
]

{ #category : #accessing }
CommandLineHandler >> description [
	^ self class description
]

{ #category : #utilities }
CommandLineHandler >> exitFailure [
	 ^ self exitFailure: 'Command line handler failed'
]

{ #category : #utilities }
CommandLineHandler >> exitFailure: aMessage [ 
	self hasSessionChanged
		ifTrue: [ ^ self ].
	Exit signalFailure: aMessage 
]

{ #category : #utilities }
CommandLineHandler >> exitSuccess [
	self hasSessionChanged
		ifTrue: [ ^ self ].
	Exit signalSuccess 
]

{ #category : #testing }
CommandLineHandler >> hasArguments [
	^ self commandLine hasArguments
]

{ #category : #'accessing arguments' }
CommandLineHandler >> hasOption: aString [
	^ self commandLine hasOption: aString
]

{ #category : #testing }
CommandLineHandler >> hasSessionChanged [
	"check whether the session has changed since the commandline handler as been created"

	^ session ~~ Smalltalk session
]

{ #category : #actions }
CommandLineHandler >> help [
	"This is a crude default help implementation."
	self printHelp.
	Smalltalk isInteractive 
		ifFalse: [ self exitSuccess ]
]

{ #category : #initialization }
CommandLineHandler >> initialize [
	super initialize.
	session := Smalltalk session.
	self 
		initializeStdout;
		initializeStderr.
	
]

{ #category : #initialization }
CommandLineHandler >> initializeStderr [
	"install the line end conversion and initialize the converter"
	stderr := VTermOutputDriver stderr.
]

{ #category : #initialization }
CommandLineHandler >> initializeStdout [
	"install the line end conversion and initialize the converter"
	stdout := VTermOutputDriver stdout.
]

{ #category : #accessing }
CommandLineHandler >> name [
	^ self printString
]

{ #category : #'accessing arguments' }
CommandLineHandler >> optionAt: aString [
	^ self commandLine optionAt: aString
]

{ #category : #'accessing arguments' }
CommandLineHandler >> optionAt: aString ifAbsent: absentBlock [
	^ self commandLine 
		optionAt: aString ifAbsent: absentBlock
]

{ #category : #'accessing arguments' }
CommandLineHandler >> optionAt: aString ifPresent: absentBlock [
	^ self commandLine 
		optionAt: aString ifPresent: absentBlock
]

{ #category : #'accessing arguments' }
CommandLineHandler >> optionAt: aString ifPresent: presentBlock ifAbsent: absentBlock [
	^ self commandLine 
		optionAt: aString ifPresent: presentBlock ifAbsent: absentBlock
]

{ #category : #printing }
CommandLineHandler >> printHelp [
	self stderr
		nextPutAll: self class comment; lf
]

{ #category : #utilities }
CommandLineHandler >> quit [
	self exitSuccess
]

{ #category : #accessing }
CommandLineHandler >> stderr [
	^ stderr
]

{ #category : #accessing }
CommandLineHandler >> stdout [
	^ stdout
]
