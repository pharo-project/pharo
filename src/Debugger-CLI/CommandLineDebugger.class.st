"
A class to execute commands from command Line.
We can parse commands and arguments, and execute the dedicated method
"
Class {
	#name : 'CommandLineDebugger',
	#superclass : 'Object',
	#instVars : [
		'isActive',
		'context',
		'process',
		'lineEnding',
		'commands',
		'commandsStack',
		'session',
		'inStream',
		'outStream',
		'historyHandlers'
	],
	#category : 'Debugger-CLI',
	#package : 'Debugger-CLI'
}

{ #category : 'instance creation' }
CommandLineDebugger class >> forCode: aString [
    "Compiles the code, create a process from it then build the debugger"
    
    | blockToDebug process |
    blockToDebug := OpalEvaluator new evaluate: '[ ' , aString , ' ]'.
    process := blockToDebug newProcess.
    
    ^ self forProcess: process
]

{ #category : 'instance creation' }
CommandLineDebugger class >> forProcess: aProcess [
    "Create a debugger from a given process"
    
    | debuggingContext debuggingSession |
    debuggingContext := aProcess suspendedContext.
    debuggingSession := DebugSession named: 'debugger' on: aProcess startedAt: debuggingContext.
    
    ^ self new
        process: aProcess;
        context: debuggingContext;
        session: debuggingSession;
        yourself
]

{ #category : 'initialization' }
CommandLineDebugger >> activate [

	isActive := true
]

{ #category : 'reading' }
CommandLineDebugger >> askTyping [

	outStream 
        << '(debug) '; 
        flush
]

{ #category : 'execution' }
CommandLineDebugger >> commandClassInstanceFor: aString [
	"Gives an instance of the right command class to use"

	[ ^ commands at: aString ]
		on: KeyNotFound
		do: [ ^ UnknownCommand new command: aString ]
]

{ #category : 'accessing' }
CommandLineDebugger >> commands [

	^ commands
]

{ #category : 'accessing' }
CommandLineDebugger >> commands: anObject [

	commands := anObject
]

{ #category : 'accessing' }
CommandLineDebugger >> commandsStack [

	^ commandsStack
]

{ #category : 'accessing' }
CommandLineDebugger >> commandsStack: anObject [

	commandsStack := anObject
]

{ #category : 'accessing' }
CommandLineDebugger >> context [

	^ context
]

{ #category : 'accessing' }
CommandLineDebugger >> context: aDebugContext [
    context := aDebugContext
]

{ #category : 'initialization' }
CommandLineDebugger >> disActivate [

	isActive := false
]

{ #category : 'execution' }
CommandLineDebugger >> executeCommand: aCommandString arg: arguments [
	"executes the command with potential arguments"

	^ (self commandClassInstanceFor: aCommandString) executeOn: self with: arguments
]

{ #category : 'execution' }
CommandLineDebugger >> finishedDebugging [

	isActive := false.
	^ 'Execution finished. The program has ended.'
]

{ #category : 'history' }
CommandLineDebugger >> handleHistory: aString [
	"Processes the input by applying history command handling through a chain of responsibility. Returns the command if a handler matches, or the original input otherwise"

	| trimmed handler |
	trimmed := aString trim.

	handler := historyHandlers detect: [ :each | each canHandle: trimmed ] ifNone: [ ^ aString ].
	^ handler handle: trimmed for: self
]

{ #category : 'execution' }
CommandLineDebugger >> handleZnIncompleteErrorInInput: ex [
	"The Stdin stream is wrapped by a Utf8 decoder, and assumes all input is encoded.
	However, this is not always the case. Sometimes we consume the buffer that has partial information, and sometimes the terminal sends not encoded text, typically for control characters (e.g., arrows and such). If we are in one of these cases, let's wait for more input and retry.
	
	Ideally, we should rewrite it in a way that we have more fine control over the decoding"

	ex class name = #ZnIncomplete
		ifTrue: [ (Delay forMilliseconds: 50) wait ]
		ifFalse: [ ex pass ]
]

{ #category : 'accessing' }
CommandLineDebugger >> inStream [

	^ inStream
]

{ #category : 'accessing' }
CommandLineDebugger >> inStream: anObject [

	inStream := anObject
]

{ #category : 'initialization' }
CommandLineDebugger >> initialize [

	super initialize.
	isActive := true.
	lineEnding := OSPlatform current lineEnding.
	self initializeCommands.
	commandsStack := Stack new.
	self initializeHistoryHandlers
]

{ #category : 'initialization' }
CommandLineDebugger >> initializeCommands [

	| commandsList |
	commandsList := {
		                QuitCommand new.
		                HelpCommand new.
		                StepCommand new.
		                OverCommand new.
		                PrintCodeCommand new.
		                PrintCommand new.
		                PrintStackCommand new.
		                HistoryCommand new }.
	commands := Dictionary new.
	commandsList do: [ :aCommand |
			commands at: aCommand commandName first put: aCommand.
			commands at: aCommand commandName last put: aCommand ]
]

{ #category : 'initialization' }
CommandLineDebugger >> initializeHistoryHandlers [

	^ historyHandlers := {
		                     LastCommandHandler new.
		                     ContansStringHandler new.
		                     NumericHistoryHandler new.
		                     StartsWithStringHandler new }
]

{ #category : 'accessing' }
CommandLineDebugger >> isActive [
    ^ isActive
]

{ #category : 'history' }
CommandLineDebugger >> lastCommand [
	"returns the last command in history and pops it"
	
	commandsStack ifNotEmpty: [ ^ commandsStack pop ]
	ifEmpty: [ ^ '' ]
	
]

{ #category : 'accessing' }
CommandLineDebugger >> lineEnding [

	^ lineEnding
]

{ #category : 'accessing' }
CommandLineDebugger >> lineEnding: anObject [

	lineEnding := anObject
]

{ #category : 'accessing' }
CommandLineDebugger >> outStream [

	^ outStream
]

{ #category : 'accessing' }
CommandLineDebugger >> outStream: anObject [

	outStream := anObject
]

{ #category : 'execution' }
CommandLineDebugger >> parsesAndExecuteCommand: aString [
	"Parses a string to change it into an executable command and executes it"

	| subString command arguments |
	commandsStack push: aString. 
	subString := aString substrings.
	subString ifEmpty: [ ^ '' ].

	command := subString first.
	arguments := subString allButFirst joinUsing: ' '.

	^ self executeCommand: command arg: arguments
]

{ #category : 'printing' }
CommandLineDebugger >> printOutput: output [

	outStream
		<< output;
		<< self lineEnding;
		flush
]

{ #category : 'accessing' }
CommandLineDebugger >> process [

	^ process
]

{ #category : 'accessing' }
CommandLineDebugger >> process: aProcess [

	process := aProcess
]

{ #category : 'reading' }
CommandLineDebugger >> readNextLine [
	"create a nil input waiting for the user to type something, handling ZnIncomplete Error. Returns the input"

	| input |
	input := nil.
	[ input isNil ] whileTrue: [
			[ input := inStream nextLine ]
				on: Error
				do: [ :ex | self handleZnIncompleteErrorInInput: ex ] ].
	^ input
]

{ #category : 'execution' }
CommandLineDebugger >> run [
	"Start debug phase, starting a loop, waiting for user's commands"

	| input tempRes |
	self printOutput: '--------------- Debug phase started ---------------'.
	self printOutput: ' For help, type "help"'.

	[ self isActive ] whileTrue: [
			self askTyping.
			input := self readNextLine.
			[

				input := self handleHistory: input trim.
				tempRes := self parsesAndExecuteCommand: input.
				self printOutput: tempRes ]
				on: HistoryEventNotFound
				do: [ :e | self printOutput: e messageText ] ]
]

{ #category : 'accessing' }
CommandLineDebugger >> session [

	^ session
]

{ #category : 'accessing' }
CommandLineDebugger >> session: anObject [

	session := anObject
]
