"
I represent the (stateful) context in which command parsing & execution are happening.
My specification is the root command or parameter that will be matched against the the sequence of arguments I receive.
To enable dependency injection, all command code should ask me for any external resource (e.g. exiting, error handling, I/O streams).

For example, the command invocation ""eval '1 + 2' "" is represented by this:
	ClapContext on: #('eval' '1 + 2')

"
Class {
	#name : #ClapContext,
	#superclass : #ClapExpression,
	#instVars : [
		'arguments',
		'obeyingExits',
		'stdio',
		'match',
		'leftovers',
		'session'
	],
	#category : #'Clap-Core-Activation'
}

{ #category : #accessing }
ClapContext class >> defaultRoot [
	^ (ClapRoot withName: 'clap')
		description: 'Entry point for commands implemented with Clap';
		add: ClapFlag forHelp;
		add: ClapCommand forHelp;
		meaning: [ :args |
			args atName: 'help' ifFound: [ :help |
				help value; exitSuccess ].
			args validateAll ];
		yourself
	"setup default flags, omnipresent subcommands & basic meanings here"
]

{ #category : #accessing }
ClapContext class >> pragmaCommands [
	^ (PragmaCollector filter: [:prg | prg selector = 'commandline']) reset
		collect: [ :pragma |
			| theClass theSelector |
			theClass := pragma method methodClass.
			theSelector := pragma method selector.
			self assert: [ theSelector isUnary ].
			
			theClass instanceSide
				perform: theSelector ]
]

{ #category : #'instance creation' }
ClapContext class >> withAll: commandCandidates [
	^ self specification: (self defaultRoot addAll: commandCandidates; yourself)
]

{ #category : #'instance creation' }
ClapContext class >> withPragmaCommands [ 
	^ self withAll: self pragmaCommands
]

{ #category : #activation }
ClapContext >> activateWith: args [
	^ self
		activateWith: args
		onExit: [ :exit | self handleExit: exit ]
]

{ #category : #activation }
ClapContext >> activateWith: args onExit: exitBlock [
	^ [ self
			rememberSession;
			interpret: args
	]
	on: Exit
	do: exitBlock
]

{ #category : #validation }
ClapContext >> allValidations [
	^ self validateOn: ClapValidationReport success.

]

{ #category : #accessing }
ClapContext >> arguments [
	^ arguments
]

{ #category : #initialization }
ClapContext >> arguments: aCollection [
	arguments := aCollection
]

{ #category : #initialization }
ClapContext >> beObeyingExits [
	obeyingExits := true
]

{ #category : #streams }
ClapContext >> binaryStderr [
	^ self stdio stderr
]

{ #category : #streams }
ClapContext >> binaryStdin [
	^ self stdio stdin
]

{ #category : #streams }
ClapContext >> binaryStdout [
	^ self stdio stdout
]

{ #category : #accessing }
ClapContext >> context [
	^ self
]

{ #category : #matching }
ClapContext >> doMatch [
	| args |
	args := self arguments readStream.
	match := self specification matchOn: args.
	match parent: self.
	leftovers := args upToEnd.
	^ match
]

{ #category : #accessing }
ClapContext >> documenter [
	^ ClapDocumenter on: self stdout
]

{ #category : #running }
ClapContext >> exit: status [
	^ (Exit status: status) signal
]

{ #category : #running }
ClapContext >> exit: status message: message [
	^ (Exit status: status) signal: message
]

{ #category : #running }
ClapContext >> exitFailure [
	^ Exit signalFailure
]

{ #category : #running }
ClapContext >> exitFailure: message [
	^ Exit signalFailure: message
]

{ #category : #running }
ClapContext >> exitSuccess [
	^ Exit signalSuccess
]

{ #category : #running }
ClapContext >> exitSuccess: message [
	^ Exit signalSuccess: message
]

{ #category : #activation }
ClapContext >> handleExit: exit [
	self hasSessionChanged ifTrue: [ ^ self ].

	self shouldObeyExit
		ifTrue: [ exit pass ]
		ifFalse: [
			exit isSuccess
				ifFalse: [ exit resignalAs: (Error new messageText: exit messageText) ] ]
]

{ #category : #activation }
ClapContext >> hasSessionChanged [
	^ session ~~ Smalltalk session
]

{ #category : #testing }
ClapContext >> ifMatch: matchBlock ifMismatch: failBlock [
	^ self match
		ifMatch: matchBlock
		ifMismatch: failBlock

]

{ #category : #initialization }
ClapContext >> initialize [
	arguments := #().
	obeyingExits := false
]

{ #category : #activation }
ClapContext >> interpret: argumentsSequence [
	^ self arguments: argumentsSequence;
		ifMatch: [ :matched |
			matched value: self.
			self exitSuccess ]
		ifMismatch: [ self noneMatched ]
]

{ #category : #accessing }
ClapContext >> leftovers [
	^ leftovers
]

{ #category : #accessing }
ClapContext >> match [
	^ match ifNil: [ self doMatch ]

]

{ #category : #activation }
ClapContext >> noneMatched [
	^ self exitFailure: 'Unknown command'
]

{ #category : #printing }
ClapContext >> printOn: aStream [
	super printOn: aStream.
	aStream
		nextPut: $(;
		print: arguments;
		nextPut: $)
]

{ #category : #activation }
ClapContext >> rememberSession [
	session := Smalltalk session
]

{ #category : #testing }
ClapContext >> shouldObeyExit [
	^ obeyingExits and: [ Smalltalk isInteractive not ]
]

{ #category : #streams }
ClapContext >> stderr [
	^ self stderrEncoded: 'utf8'
]

{ #category : #streams }
ClapContext >> stderrEncoded: anEncoding [
	^ ZnNewLineWriterStream on:
		(ZnCharacterWriteStream on: self binaryStderr encoding: anEncoding)
]

{ #category : #streams }
ClapContext >> stdin [
	^ self stdinEncoded: 'utf8'
]

{ #category : #streams }
ClapContext >> stdinEncoded: anEncoding [
	^ ZnCharacterReadStream
		on: self binaryStdin
		encoding: anEncoding
]

{ #category : #streams }
ClapContext >> stdio [
	^ stdio ifNil: [ Stdio ]
]

{ #category : #streams }
ClapContext >> stdout [
	^ self stdoutEncoded: 'utf8'
]

{ #category : #streams }
ClapContext >> stdoutEncoded: anEncoding [
	^ ZnNewLineWriterStream on:
		(ZnCharacterWriteStream on: self binaryStdout encoding: anEncoding)
]

{ #category : #validation }
ClapContext >> validateAll [
	| report |
	report := self allValidations.
	report isSuccess ifTrue: [ ^ self ].
	
	self stderr print: report; flush.
	self exitFailure
]

{ #category : #validation }
ClapContext >> validateOn: aReport [
	match validateOn: aReport.
	aReport add: (ClapLeftoversValidation new validate: self).

	^ aReport
]
