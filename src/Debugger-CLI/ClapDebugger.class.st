"
A class to execute commands from command Line.
We can parse commands and arguments, and execute the dedicated method
"
Class {
	#name : 'ClapDebugger',
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
ClapDebugger class >> forCode: aString [
    "Compiles the code, create a process from it then build the debugger"
    
    | blockToDebug process |
    blockToDebug := OpalEvaluator new evaluate: '[ ' , aString , ' ]'.
    process := blockToDebug newProcess.
    
    ^ self forProcess: process
]

{ #category : 'instance creation' }
ClapDebugger class >> forProcess: aProcess [
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
ClapDebugger >> activate [

	isActive := true
]

{ #category : 'reading' }
ClapDebugger >> askTyping [

	outStream 
        << '(debug) '; 
        flush
]

{ #category : 'execution' }
ClapDebugger >> commandClassInstanceFor: aString [
	"Gives an instance of the right command class to use"

	[ ^ commands at: aString ]
		on: KeyNotFound
		do: [ ^ ClapDbgUnknownCommand new command: aString ]
]

{ #category : 'accessing' }
ClapDebugger >> commands [

	^ commands
]

{ #category : 'accessing' }
ClapDebugger >> commands: anObject [

	commands := anObject
]

{ #category : 'accessing' }
ClapDebugger >> commandsStack [

	^ commandsStack
]

{ #category : 'accessing' }
ClapDebugger >> commandsStack: anObject [

	commandsStack := anObject
]

{ #category : 'accessing' }
ClapDebugger >> context [

	^ context
]

{ #category : 'accessing' }
ClapDebugger >> context: aDebugContext [
    context := aDebugContext
]

{ #category : 'initialization' }
ClapDebugger >> disActivate [

	isActive := false
]

{ #category : 'command shortcut' }
ClapDebugger >> down [
	
	^ self executeCommand: 'down' arg: ''
]

{ #category : 'execution' }
ClapDebugger >> executeCommand: aCommandString arg: arguments [
	"executes the command with potential arguments"

	^ (self commandClassInstanceFor: aCommandString) executeOn: self with: arguments
]

{ #category : 'command shortcut' }
ClapDebugger >> finish [

	^ self executeCommand: 'finish' arg: ''
]

{ #category : 'execution' }
ClapDebugger >> finishedDebugging [

	isActive := false.
	^ 'Execution finished. The program has ended.'
]

{ #category : 'history' }
ClapDebugger >> handleHistory: aString [
	"Processes the input by applying history command handling through a chain of responsibility. Returns the command if a handler matches, or the original input otherwise"

	| trimmed handler |
	trimmed := aString trim.

	handler := historyHandlers detect: [ :each | each canHandle: trimmed ] ifNone: [ ^ aString ].
	^ handler handle: trimmed for: self
]

{ #category : 'execution' }
ClapDebugger >> handleZnIncompleteErrorInInput: ex [
	"The Stdin stream is wrapped by a Utf8 decoder, and assumes all input is encoded.
	However, this is not always the case. Sometimes we consume the buffer that has partial information, and sometimes the terminal sends not encoded text, typically for control characters (e.g., arrows and such). If we are in one of these cases, let's wait for more input and retry.
	
	Ideally, we should rewrite it in a way that we have more fine control over the decoding"

	ex class name = #ZnIncomplete
		ifTrue: [ (Delay forMilliseconds: 50) wait ]
		ifFalse: [ ex pass ]
]

{ #category : 'accessing' }
ClapDebugger >> inStream [

	^ inStream
]

{ #category : 'accessing' }
ClapDebugger >> inStream: anObject [

	inStream := anObject
]

{ #category : 'initialization' }
ClapDebugger >> initialize [

	super initialize.
	isActive := true.
	lineEnding := OSPlatform current lineEnding.
	self initializeCommands.
	commandsStack := Stack new.
	self initializeHistoryHandlers
]

{ #category : 'initialization' }
ClapDebugger >> initializeCommands [

	| commandsList |
	commandsList := {
		                ClapDbgQuitCommand new.
		                ClapDbgHelpCommand new.
		                ClapDbgStepCommand new.
		                ClapDbgOverCommand new.
		                ClapDbgPrintCodeCommand new.
		                ClapDbgPrintCommand new.
		                ClapDbgPrintStackCommand new.
		                ClapDbgHistoryCommand new.
		                ClapDbgStepThroughCommand new.
		                ClapDbgDownCommand new.
		                ClapDbgUpCommand new.
		                ClapDbgReturnCommand new.
		                ClapDbgFinishCommand new.
		                ClapDbgProceedCommand new }.
	commands := Dictionary new.
	commandsList do: [ :aCommand |
			commands at: aCommand commandName first put: aCommand.
			commands at: aCommand commandName last put: aCommand ]
]

{ #category : 'initialization' }
ClapDebugger >> initializeHistoryHandlers [

	^ historyHandlers := {
		                     ClapDbgLastCommandHandler new.
		                     ClapDbgContansStringHandler new.
		                     ClapDbgNumericHistoryHandler new.
		                     ClapDbgStartsWithStringHandler new }
]

{ #category : 'accessing' }
ClapDebugger >> isActive [
    ^ isActive
]

{ #category : 'history' }
ClapDebugger >> lastCommand [
	"returns the last command in history and pops it"
	
	commandsStack ifNotEmpty: [ ^ commandsStack pop ]
	ifEmpty: [ ^ '' ]
	
]

{ #category : 'accessing' }
ClapDebugger >> lineEnding [

	^ lineEnding
]

{ #category : 'accessing' }
ClapDebugger >> lineEnding: anObject [

	lineEnding := anObject
]

{ #category : 'accessing' }
ClapDebugger >> outStream [

	^ outStream
]

{ #category : 'accessing' }
ClapDebugger >> outStream: anObject [

	outStream := anObject
]

{ #category : 'command shortcut' }
ClapDebugger >> over [

	^ self executeCommand: 'over' arg: ''
]

{ #category : 'execution' }
ClapDebugger >> parsesAndExecuteCommand: aString [
	"Parses a string to change it into an executable command and executes it"

	| subString command arguments |
	commandsStack push: aString.
	subString := aString substrings.
	subString ifEmpty: [ ^ '' ].

	command := subString first.
	arguments := subString allButFirst joinUsing: ' '.

	^ self executeCommand: command arg: arguments
]

{ #category : 'command shortcut' }
ClapDebugger >> print: aString [ 
	^ self executeCommand: 'print' arg: aString 
]

{ #category : 'command shortcut' }
ClapDebugger >> printCode [

	^ self executeCommand: 'printCode' arg: ''
]

{ #category : 'printing' }
ClapDebugger >> printOutput: output [

	outStream
		<< output;
		<< self lineEnding;
		flush
]

{ #category : 'command shortcut' }
ClapDebugger >> printStack [

	^ self executeCommand: 'printStack' arg: ''
]

{ #category : 'command shortcut' }
ClapDebugger >> proceed [

	^ self executeCommand: 'proceed' arg: ''
]

{ #category : 'accessing' }
ClapDebugger >> process [

	^ process
]

{ #category : 'accessing' }
ClapDebugger >> process: aProcess [

	process := aProcess.
	process errorHandler: self.
]

{ #category : 'command shortcut' }
ClapDebugger >> quit [

	^ self executeCommand: 'quit' arg: ''
]

{ #category : 'reading' }
ClapDebugger >> readMultiLine [

	| stream currentLine |
	stream := WriteStream on: String new.

	currentLine := self readNextLine.

	[ currentLine endsWith: '\' ] whileTrue: [
			stream nextPutAll: (currentLine copyFrom: 1 to: currentLine size - 1) trimRight.
			stream nextPutAll: self lineEnding.
			currentLine := self readNextLine ].
	stream nextPutAll: currentLine.

	^ stream contents
]

{ #category : 'reading' }
ClapDebugger >> readNextLine [
	"create a nil input waiting for the user to type something, handling ZnIncomplete Error. Returns the input"

	| input |
	input := nil.
	[ input isNil ] whileTrue: [
			[ input := inStream nextLine ]
				on: Error
				do: [ :ex | self handleZnIncompleteErrorInInput: ex ] ].
	^ input trimRight
]

{ #category : 'command shortcut' }
ClapDebugger >> return: anInteger [

	^ self executeCommand: 'return' arg: anInteger
]

{ #category : 'execution' }
ClapDebugger >> run [
	"Start debug phase, starting a loop, waiting for user's commands"

	| input tempRes |
	self printOutput: '--------------- Debug phase started ---------------'.
	self printOutput: ' For help, type "help"'.

	[ self isActive ] whileTrue: [
			self askTyping.
			input := self readMultiLine.
			[
				input := self handleHistory: input trim.
				tempRes := self parsesAndExecuteCommand: input.
				self isActive ifFalse: [
						self printOutput: tempRes.
						^ self ].
				self printOutput: tempRes ]
				on: ClapDbgHistoryEventNotFound
				do: [ :e | self printOutput: e messageText ] ]
]

{ #category : 'accessing' }
ClapDebugger >> session [

	^ session
]

{ #category : 'accessing' }
ClapDebugger >> session: anObject [

	session := anObject
]

{ #category : 'command shortcut' }
ClapDebugger >> step [

	^ self executeCommand: 'step' arg: ''
]

{ #category : 'command shortcut' }
ClapDebugger >> stepThrough [

	^ self executeCommand: 'stepThrough' arg: ''
]

{ #category : 'command shortcut' }
ClapDebugger >> up [

	^ self executeCommand: 'up' arg: ''
]
