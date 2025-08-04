"
I am a REPL debugger and I provide an access to the Sindarin debugging API.
I do not depend on morphic, and while I depend on Color, I can work without it.

I use a `ReadWriteStream` to handle user input and output my results.
I use `Display` and `DisplayScreen` to show my results.

To invoke me, use my class method `debugError: anError fromSession: aDebugSession`.
This method takes as input an error and a debug session.
The error is used to display information about what brought you to ED.
It is not mandatory, if you only have a debug session you can invoke me by doing:
`EDEmergencyDebugger new emergencyDebug: aDebugSession`.
The debug session contains all information necessary to debug.

For example, execute the following as an example:
```
|context process session|
context := [ EDMockObjectForTests new m: 20 ] asContext.
	process := Process
		forContext: context
		priority: Processor userInterruptPriority.
	session := DebugSession
		named: 'test session'
		on: process
		startedAt: context.
	session stepThrough.
	session stepOver.
	session stepInto.
	session stepOver.
	session stepOver.
	session stepInto.
	EDEmergencyDebugger new emergencyDebug: session.
```
	
Navigate the stack with up/down arrows.
When you are on a method (in the stack), use left/right arrow to navigate the versions of that method.
To revert the selected method (in the stack) to the selected version (displayed in the middle text pane), type `revert` and press enter.

Don't forget to read the help text, type `h` or `help` to display the help text.

"
Class {
	#name : 'EDEmergencyDebugger',
	#superclass : 'Object',
	#traits : 'TDebugger',
	#classTraits : 'TDebugger classTrait',
	#instVars : [
		'rawDisplay',
		'stackSelectionIndex',
		'methodVersionSelectionIndex',
		'methodText',
		'stackText',
		'titleText',
		'repl',
		'input',
		'result',
		'isRunning',
		'debugAPI',
		'actions',
		'actionsDescriptions',
		'shortStack',
		'shortStackIndex',
		'error',
		'errorText'
	],
	#classVars : [
		'DefaultDebugAPI'
	],
	#category : 'EmergencyDebugger-Core',
	#package : 'EmergencyDebugger',
	#tag : 'Core'
}

{ #category : 'accessing' }
EDEmergencyDebugger class >> availableAutomatically [
	^ true
]

{ #category : 'utilities api' }
EDEmergencyDebugger class >> closeAllDebuggers [
	"Do nothing, because this message does not make sense for the Emergency Debugger. If the emergencyDebugger  is open, it has the focus so the user cannot right-click on the taskbar and click on the 'close all debuggers' button anyway"

	^ self
]

{ #category : 'instance creation' }
EDEmergencyDebugger class >> debug: aDebugSession [
	self flag: 'Not good, must add and test the error too!'.
	^ self new emergencyDebug: aDebugSession
]

{ #category : 'accessing' }
EDEmergencyDebugger class >> debugAPI [
	^self defaultDebugAPI
]

{ #category : 'accessing' }
EDEmergencyDebugger class >> debugAPI: aDebugAPIClass [
	DefaultDebugAPI := aDebugAPIClass
]

{ #category : 'instance creation' }
EDEmergencyDebugger class >> debugError: anError fromSession: aDebugSession [
	^ self new
		fromError: anError;
		emergencyDebug: aDebugSession
]

{ #category : 'instance creation' }
EDEmergencyDebugger class >> debugSession: aDebugSession [

	^ self debug: aDebugSession
]

{ #category : 'accessing' }
EDEmergencyDebugger class >> defaultDebugAPI [
	^ DefaultDebugAPI ifNil: [ DefaultDebugAPI := EDDebuggingAPI ]
]

{ #category : 'actions' }
EDEmergencyDebugger >> availableActions [
	^actions ifNil:[actions := self buildDefaultActions]
]

{ #category : 'actions' }
EDEmergencyDebugger >> availableActionsDescriptions [
	^actionsDescriptions ifNil:[actionsDescriptions := self buildDefaultActionsDescriptions]
]

{ #category : 'actions' }
EDEmergencyDebugger >> availableActionsDescriptionsMap [
	^self buildDefaultActionsDescriptionsMap
]

{ #category : 'initialization' }
EDEmergencyDebugger >> basicDebug: aDebugSession [
	debugAPI := self class debugAPI attachTo: aDebugSession.
	debugAPI debugger: self.
	isRunning := true
]

{ #category : 'actions' }
EDEmergencyDebugger >> buildDefaultActions [
	|dic|

	dic := Dictionary new.
	dic at: 'q!' 						put: [ debugAPI closeEmergencySession ].
	dic at: 'q' 							put: [ debugAPI terminateSession ].
	dic at: 'showDebuggerError' 	put: [ self showDebuggerError ].
	dic at: 'exit' 						put: [ debugAPI terminateSession ].
	dic at: 'retry' 					put: [ debugAPI tryReopenSessionWithDebugger ].
	dic at: Character arrowUp 		put: [ self moveUpInStack ].
	dic at: Character arrowDown 	put: [ self moveDownInStack ].
	dic at: Character arrowLeft 	put: [ self showMoreRecentMethodVersion ].
	dic at: Character arrowRight 	put: [ self showOlderMethodVersion ].
	dic at: 'help' 						put: [ self composeHelpText ].
	dic at: 'revert' 					put: [ self revertCurrentMethodToSelectedVersion ].
	dic at: 'forceRevert' 			put: [ self revertCurrentMethodToSelectedVersionAndQuit ].
	dic at: 'terminateAll' 			put: [ debugAPI terminateAllProcesses ].
	dic at: 'proceed' 					put: [ debugAPI resume ].
	dic at: 'debug' 					put: [ self debugDebuggerError ].
	dic at: 'dump' 						put: [ self dumpStackAndQuit ].
	dic at: 'inspect' 					put: [ self inspectStackAndQuit ].

	^ dic
]

{ #category : 'actions' }
EDEmergencyDebugger >> buildDefaultActionsDescriptions [
	|dic|
	dic := Dictionary new.
	dic at: 'exit/q' put: 'Exit the emergency debugger'.
	dic at: 'help' put: 'Show this help.'.
	dic at: 'retry' put: 'Retry opening a graphical debugger'.
	dic at: '<UP/DOWN> arrows' put: 'Move up/down in the stack'.
	dic at: '<RIGHT/LEFT> arrows' put: 'Navigate the current method versions'.
	dic at: 'revert' put: 'Revert the current method to the selected method version in the current context'.
	dic at: 'forceRevert' put: 'Recompiles the current method to the selected method version and quits the emergency debugger'.
	dic at: 'terminateAll' put: 'Terminate all non-essential processes (will terminate Ed)'.
	dic at: 'proceed' put: 'Proceeds the execution'.
	dic at: 'showDebuggerError' put: 'Display debugger error stack, i.e., why you are here.'.
	dic at: 'debug' put: 'Debug the debugger error.'.
	dic at: 'dump' put: 'Dump a printed representation of the stack, quits the debugger and inspects the dump.'.
	dic at: 'inspect' put: 'Quits the debugger and inspects the stack.'.
	^dic
]

{ #category : 'actions' }
EDEmergencyDebugger >> buildDefaultActionsDescriptionsMap [
	|dic|
	dic := Dictionary new.
	dic at: 'exit' put: 'exit/q'.
	dic at: 'q' put: 'exit/q'.
	dic at: 'retry' put: 'retry'.
	dic at: Character arrowUp put: '<UP/DOWN> arrows'.
	dic at: Character arrowDown put: '<UP/DOWN> arrows'.
	dic at: Character arrowLeft put: '<RIGHT/LEFT> arrows'.
	dic at: Character arrowRight put: '<RIGHT/LEFT> arrows'.
	dic at: 'revert' put: 'revert'.
	dic at: 'forceRevert' put: 'forceRevert'.
	dic at: 'help' put: 'help'.
	dic at: 'terminateAll' put: 'terminateAll'.
	dic at: 'proceed' put: 'proceed'.
	dic at: 'showDebuggerError' put: 'showDebuggerError'.
	dic at: 'debug' put: 'debug'.
	dic at: 'dump' put: 'dump'.
	dic at: 'inspect' put: 'inspect'.
	^dic
]

{ #category : 'text - composing' }
EDEmergencyDebugger >> composeDisplayText [
	^(titleText , (self composeSessionTitle), stackText, methodText, input, result )
]

{ #category : 'text - composing' }
EDEmergencyDebugger >> composeErrorTextFrom: anError [
	| errStream |
	errStream := WriteStream on: Text new.
	errStream cr.
	self writeSeparatorOn: errStream.
	errStream << anError description.
	errorText := errStream contents
]

{ #category : 'text - composing' }
EDEmergencyDebugger >> composeHelpText [
	| str |
	str := self newTextStream.
	self availableActionsDescriptions
		keysAndValuesDo: [ :cmd :desc |
			|text|
			text := (cmd, ': ') asText allBold, desc asText.
			text makeAllColor: rawDisplay gray.
			str << text.
			str cr ].
	^ self composeResult: str contents title: 'HELP'
]

{ #category : 'text - composing' }
EDEmergencyDebugger >> composeMethodText [
	| str method node text |
	str := self newTextStream.
	self writeSectionTitle: 'SOURCE' on: str.
	method := debugAPI methodAt: stackSelectionIndex.
	node := debugAPI nodeAt: stackSelectionIndex.
	text := method sourceCode asText.
	text makeColor: rawDisplay red from: node start to: node stop.
	str << method methodClass name.
	str << '>>'.
	str << text.
	str cr.
	methodText := str contents
]

{ #category : 'text - composing' }
EDEmergencyDebugger >> composePromptWith: aString [
	| str  |
	str := self newTextStream.
	str cr.
	self writeSeparatorOn: str.
	str << '> ' asText allBold.
	str << aString trim asText.
	input := str contents
]

{ #category : 'text - composing' }
EDEmergencyDebugger >> composeResult: text title: title [
	| str |
	str := self newTextStream.
	str cr.
	self writeSectionTitle: title on: str.
	str << text.
	result := str contents
]

{ #category : 'text - composing' }
EDEmergencyDebugger >> composeSessionTitle [
	| errStream |
	errStream := WriteStream on: Text new.
	errStream cr.
	self writeSeparatorOn: errStream.
	errStream << debugAPI session name.
	^errStream contents
]

{ #category : 'text - composing' }
EDEmergencyDebugger >> composeStackText [
	| str stack |
	str := self newTextStream.
	stack := self shortStack.
	str cr.
	self
		writeSectionTitle:
			'STACK (' , stackSelectionIndex printString , '/'
				, debugAPI stackSize printString , ')'
		on: str.
	1 to: stack size do: [ :i |
		| textStream text |
		textStream := self newTextStream.
		(stack at: i) printWithArgsValueOn: textStream.
		text := textStream contents.
		i = shortStackIndex
			ifTrue: [ text allBold ].
		str << text.
		str cr ].
	stackText := str contents
]

{ #category : 'text - composing' }
EDEmergencyDebugger >> composeTitleText [
	titleText := 'Hi, I''m ED - What is the nature of your debugging emergency?' asText allBold
]

{ #category : 'initialization' }
EDEmergencyDebugger >> debug: aDebugSession [

	self basicDebug: aDebugSession.
	self composeTitleText.
	self composeStackText.
	self composeMethodText.
	self composeHelpText.
	self composePromptWith: ''.
	self updateDisplay.
	repl readEvalPrint
]

{ #category : 'accessing' }
EDEmergencyDebugger >> debugAPI [
	^debugAPI
]

{ #category : 'accessing' }
EDEmergencyDebugger >> debugAPI: aDebugAPI [
	debugAPI := aDebugAPI
]

{ #category : 'private' }
EDEmergencyDebugger >> debugDebuggerError [
	self terminate.
	error debug
]

{ #category : 'text - helpers' }
EDEmergencyDebugger >> defaultResultText [
	^ 'TYPE h FOR HELP' asText
]

{ #category : 'actions' }
EDEmergencyDebugger >> descriptionFromPromptCommand: aString [
	|key|
	key := self availableActionsDescriptionsMap at: aString.
	^self availableActionsDescriptions at: key
]

{ #category : 'accessing' }
EDEmergencyDebugger >> displayInterface: aDisplayInterface [
	rawDisplay := aDisplayInterface
]

{ #category : 'private' }
EDEmergencyDebugger >> dumpStackAndQuit [
	| stackStrace |
	stackStrace := debugAPI dumpStack.
	debugAPI terminateSession.
	stackStrace inspect
]

{ #category : 'initialization' }
EDEmergencyDebugger >> emergencyDebug: aDebugSession [

	[ self debug: aDebugSession ]
		on: Error
		do: [ :err |
			DebuggerEmmergencyLogger new
				logError: err
				forSession: aDebugSession ]
]

{ #category : 'error handling' }
EDEmergencyDebugger >> errorDescriptionFrom: anError [
	^ [ anError description ]
		on: Error
		do: [ 'a ' , anError class printString ]
]

{ #category : 'accessing' }
EDEmergencyDebugger >> errorText [
	^errorText
]

{ #category : 'accessing' }
EDEmergencyDebugger >> errorText: aText [
	errorText := aText
]

{ #category : 'evaluation' }
EDEmergencyDebugger >> eval: command [

	self evalCommand: command.
	self composePromptWith: ''.
	self updateDisplay
]

{ #category : 'evaluation' }
EDEmergencyDebugger >> evalCommand: aString [
	| action |

	action := self availableActions at: aString ifAbsent: [ ^ false ].
	[ action value ]
		on: Exception
		do:
			[ :e | self handleEmergencyDebuggerError: e description: aString ].
	self updateDisplay.
	^ true
]

{ #category : 'initialization' }
EDEmergencyDebugger >> fromError: anError [
	self composeErrorTextFrom: anError.
	error := anError
]

{ #category : 'error handling' }
EDEmergencyDebugger >> handleEmergencyDebuggerError: e description: commandString [
	self
		composeResult:
			('Cannot ' , (self descriptionFromPromptCommand: commandString))
				asText allBold
		title: 'ERROR: ' , (self errorDescriptionFrom: e)
	"isRunning := false.
	[ [ e debug ]
		on: Error
		do: [ :err | self signalDebuggerError: err forSession: debugAPI session ] ]
		ensure: [ isRunning := true.
			repl readEvalPrint ]"
]

{ #category : 'initialization' }
EDEmergencyDebugger >> initialize [

	repl := EDREPLInterface forDebugger: self.
	rawDisplay := EDDisplayInterface new
		whenCharacterReceivedDo: [ :aChar | repl pushChar: aChar ];
		whenWindowClosedDo: [ self terminate ].

	stackSelectionIndex := 1.
	shortStackIndex := 1.
	methodVersionSelectionIndex := 0.
	input := Text new.
	self composeResult: Text new title: self defaultResultText.
	isRunning := false.
	errorText := Text new
]

{ #category : 'accessing' }
EDEmergencyDebugger >> input [
	^input
]

{ #category : 'private' }
EDEmergencyDebugger >> inspectStackAndQuit [
	debugAPI terminateSession.
	debugAPI longStack inspect
]

{ #category : 'testing' }
EDEmergencyDebugger >> isRunning [
	^isRunning
]

{ #category : 'accessing' }
EDEmergencyDebugger >> methodText [
	^methodText
]

{ #category : 'accessing' }
EDEmergencyDebugger >> methodVersionSelectionIndex [
	^methodVersionSelectionIndex
]

{ #category : 'accessing' }
EDEmergencyDebugger >> methodVersionSelectionIndex: anInteger [
	methodVersionSelectionIndex := anInteger
]

{ #category : 'stack' }
EDEmergencyDebugger >> moveDownInStack [
	stackSelectionIndex = debugAPI stackSize
		ifTrue: [ ^ self ].
	stackSelectionIndex := stackSelectionIndex + 1.
	shortStackIndex := shortStackIndex + 1.
	shortStackIndex > debugAPI displayStackSize
		ifTrue: [ self shiftDownShortStack ].
	methodVersionSelectionIndex := 0.
	self composeStackText.
	self composeMethodText.
	self composeHelpText
]

{ #category : 'stack' }
EDEmergencyDebugger >> moveUpInStack [
	stackSelectionIndex = 1
		ifTrue: [ ^ self ].
	stackSelectionIndex := stackSelectionIndex - 1.
	shortStackIndex := shortStackIndex - 1.
	shortStackIndex = 0
		ifTrue: [ self shiftUpShortStack ].
	methodVersionSelectionIndex := 0.
	self composeStackText.
	self composeMethodText.
	self composeHelpText
]

{ #category : 'text - helpers' }
EDEmergencyDebugger >> newTextStream [
	^WriteStream on: Text new
]

{ #category : 'accessing' }
EDEmergencyDebugger >> originalError [
	^error
]

{ #category : 'evaluation' }
EDEmergencyDebugger >> performActionForChar: aCharacter [
	^self evalCommand: aCharacter
]

{ #category : 'accessing' }
EDEmergencyDebugger >> repl: aREPLInterface [
	repl := aREPLInterface
]

{ #category : 'accessing' }
EDEmergencyDebugger >> resetDisplay [

]

{ #category : 'accessing' }
EDEmergencyDebugger >> result [
	^result
]

{ #category : 'accessing' }
EDEmergencyDebugger >> resultText [
	^ result
]

{ #category : 'methods' }
EDEmergencyDebugger >> revertCurrentMethodToSelectedVersion [
	"should:
		- revert method from the current selected context to the selected method version
		- restart the context
		- update texts"
	|method methodVersion|
	method := debugAPI methodAt: stackSelectionIndex.
	methodVersion := (debugAPI methodVersionAt: stackSelectionIndex) at: methodVersionSelectionIndex.
	debugAPI revert: method to: methodVersion inContext: (debugAPI contextAt: stackSelectionIndex).
	shortStack := nil.

	stackSelectionIndex := 1.
	shortStackIndex := 1.
	self composeStackText.
	self composeMethodText.
	methodVersionSelectionIndex := 1.
	self showSelectedMethodVersion
]

{ #category : 'methods' }
EDEmergencyDebugger >> revertCurrentMethodToSelectedVersionAndQuit [
	"should:
		- revert method from the current selected context to the selected method version
		- quit the emergency debugger"
	|method methodVersion|
	method := debugAPI methodAt: stackSelectionIndex.
	methodVersion := (debugAPI methodVersionAt: stackSelectionIndex) at: methodVersionSelectionIndex.
	debugAPI revert: method to: methodVersion inContext: (debugAPI contextAt: stackSelectionIndex).
	debugAPI terminateSession
]

{ #category : 'methods' }
EDEmergencyDebugger >> selectedMethodVersion [
	|versions |
	versions := debugAPI methodVersionAt: stackSelectionIndex.
	^versions at: methodVersionSelectionIndex
]

{ #category : 'methods' }
EDEmergencyDebugger >> selectedMethodVersionsSize [
	^ debugAPI methodVersionSizeAt: stackSelectionIndex
]

{ #category : 'text - helpers' }
EDEmergencyDebugger >> separator [
	^'--------------------' asText makeAllColor: rawDisplay gray
]

{ #category : 'stack' }
EDEmergencyDebugger >> shiftDownShortStack [
	"rebuilds the short stack that is displayed by shitfing down the stack view by debugAPI displayStackSize elements"

	| start stop |
	shortStackIndex := 1.
	start := stackSelectionIndex.
	stop := stackSelectionIndex + debugAPI displayStackSize - 1
		min: debugAPI stackSize.
	shortStack := debugAPI stackFrom: start to: stop
]

{ #category : 'stack' }
EDEmergencyDebugger >> shiftUpShortStack [
	"rebuilds the short stack that is displayed by shitfing up the stack view by debugAPI displayStackSize elements"

	stackSelectionIndex = 1
		ifTrue: [ ^ shortStack := debugAPI stackFrom: 1 to: debugAPI displayStackSize ].
	shortStackIndex := debugAPI displayStackSize.
	^ shortStack := debugAPI
		stackFrom: (stackSelectionIndex - debugAPI displayStackSize max: 1)
		to: stackSelectionIndex
]

{ #category : 'stack' }
EDEmergencyDebugger >> shortStack [
	^ shortStack
		ifNil: [ shortStack := debugAPI stackFrom: 1 to: debugAPI displayStackSize ]
]

{ #category : 'accessing' }
EDEmergencyDebugger >> shortStackIndex [
	^shortStackIndex
]

{ #category : 'accessing' }
EDEmergencyDebugger >> shortStackIndex: newIndex [
	shortStackIndex := newIndex
]

{ #category : 'private' }
EDEmergencyDebugger >> showDebuggerError [

	| context text |
	error ifNil: [
		self composeResult: '' title: 'No error.'.
		^ self updateDisplay ].
	context := error signalerContext.
	text := String streamContents: [ :s |
		        20 timesRepeat: [
			        context ifNotNil: [
				        s
					        cr;
					        print: (context := context sender) ] ] ].
	self composeResult: text title: error description.
	self updateDisplay
]

{ #category : 'methods' }
EDEmergencyDebugger >> showMoreRecentMethodVersion [
	methodVersionSelectionIndex := methodVersionSelectionIndex - 1 max: 1.
	self showSelectedMethodVersion
]

{ #category : 'methods' }
EDEmergencyDebugger >> showOlderMethodVersion [
	methodVersionSelectionIndex := methodVersionSelectionIndex + 1
		min: self selectedMethodVersionsSize.
	self showSelectedMethodVersion
]

{ #category : 'methods' }
EDEmergencyDebugger >> showSelectedMethodVersion [
	| selectedVersion text |
	selectedVersion := self selectedMethodVersion.
	text := selectedVersion sourceCode asText.
	text makeBoldFrom: 1 to: selectedVersion methodSelector size.
	self
		composeResult: text
		title:
			'VERSIONS (' , methodVersionSelectionIndex printString , '/'
				, self selectedMethodVersionsSize printString , ')'.
	self updateDisplay
]

{ #category : 'accessing' }
EDEmergencyDebugger >> stackSelectionIndex [
	^stackSelectionIndex
]

{ #category : 'accessing' }
EDEmergencyDebugger >> stackSelectionIndex: newIndex [
	stackSelectionIndex := newIndex
]

{ #category : 'accessing' }
EDEmergencyDebugger >> stackText [
	^stackText
]

{ #category : 'initialization' }
EDEmergencyDebugger >> terminate [

	isRunning := false.
	rawDisplay terminateWindow.
	repl terminateREPL
]

{ #category : 'accessing' }
EDEmergencyDebugger >> titleText [
	^titleText
]

{ #category : 'updating' }
EDEmergencyDebugger >> updateDisplay [
	rawDisplay clear.
	rawDisplay show: self composeDisplayText
]

{ #category : 'updating' }
EDEmergencyDebugger >> updatePrompt: aString [
	self composePromptWith: aString.
	self updateDisplay
]

{ #category : 'text - helpers' }
EDEmergencyDebugger >> writeSectionTitle: aString on: aStream [
	self writeSeparatorOn: aStream.
	aStream << (aString asText makeAllColor: rawDisplay gray).
	aStream cr.
	self writeSeparatorOn: aStream
]

{ #category : 'text - helpers' }
EDEmergencyDebugger >> writeSeparatorOn: aStream [
	aStream << self separator.
	aStream cr
]
