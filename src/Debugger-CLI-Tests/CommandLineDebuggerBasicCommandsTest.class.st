Class {
	#name : 'CommandLineDebuggerBasicCommandsTest',
	#superclass : 'TestCase',
	#instVars : [
		'debugger'
	],
	#category : 'Debugger-CLI-Tests',
	#package : 'Debugger-CLI-Tests'
}

{ #category : 'running' }
CommandLineDebuggerBasicCommandsTest >> setUp [

	| debuggingProcess debuggingContext debuggingSession |
	super setUp.
	debuggingProcess := [ self class compiler evaluate: 'OCParser parseMethod: foo self' ] newProcess.
	debuggingContext := debuggingProcess suspendedContext.
	debuggingSession := DebugSession named: 'test' on: debuggingProcess startedAt: debuggingContext.
	debugger := CommandLineDebugger new
		            context: debuggingContext;
		            process: debuggingProcess;
		            session: debuggingSession
]

{ #category : 'tests' }
CommandLineDebuggerBasicCommandsTest >> setUpContextForPrintCode [

	| currentContext |
	currentContext := Context
		                  sender: nil
		                  receiver: MockForDebugger new
		                  method: MockForDebugger >> #methodeToPrintCode
		                  arguments: #(  ).
	debugger context: currentContext
]

{ #category : 'running' }
CommandLineDebuggerBasicCommandsTest >> setUpContextForPrinting [

	| currentContext |
	currentContext := Context
		                  sender: nil
		                  receiver: MockForDebugger new
		                  method: MockForDebugger >> #methodToPrintCode:withArgument:
		                  arguments: #( 1 32 ).
	debugger context: currentContext
]

{ #category : 'running' }
CommandLineDebuggerBasicCommandsTest >> setUpDebuggerOnNavigationMock [

	| process context session |
	process := [ MockForDebugger new methodForNavigationInACode ] newProcess.
	context := process suspendedContext.

	session := DebugSession named: 'TestNav' on: process startedAt: context.

	debugger
		process: process;
		session: session;
		context: context.

	[ debugger context method selector = #methodForNavigationInACode ] whileFalse: [ debugger parsesAndExecuteCommand: 'step' ].
	^ debugger context
]

{ #category : 'tests' }
CommandLineDebuggerBasicCommandsTest >> testEmptyCommandDoesNothing [

	| response |
	response := debugger parsesAndExecuteCommand: '          '.

	self assert: response equals: ''
]

{ #category : 'tests' }
CommandLineDebuggerBasicCommandsTest >> testOverCommandAdvancesPCOnSameContext [
	
	| initialContext initialPc |

	initialContext := self setUpDebuggerOnNavigationMock.
	initialPc := initialContext pc.
	debugger parsesAndExecuteCommand: 'over'.
	self assert: debugger context method equals: initialContext method.
	self deny: debugger context pc equals: initialPc.
]

{ #category : 'tests' }
CommandLineDebuggerBasicCommandsTest >> testPrintCodeCommandShowsMethodSourceAndHighlightsCurrentInstruction [

    | response |
    self setUpContextForPrintCode.
    
    response := debugger parsesAndExecuteCommand: 'printCode'.

    self assert: (response includesSubstring: 'methodeToPrintCode').
    self assert: (response includesSubstring: '"Fake method to test printCode"').
    self assert: (response includesSubstring: 'a := --->42<---.').
    self assert: (response includesSubstring: 'b := a + 1.').
    self assert: (response includesSubstring: '^ b * 2').
    
    self assert: debugger isActive
]

{ #category : 'tests' }
CommandLineDebuggerBasicCommandsTest >> testPrintCommandEvaluatesArgumentsCorrectly [

	| response1 response2 |
	self setUpContextForPrinting.

	response1 := debugger parsesAndExecuteCommand: 'print arg1'.
	self assert: response1 equals: 'arg1 : 1'.

	response2 := debugger parsesAndExecuteCommand: 'print arg2'.
	self assert: response2 equals: 'arg2 : 32'
]

{ #category : 'tests' }
CommandLineDebuggerBasicCommandsTest >> testPrintCommandWithDivisionByZeroReturnsError [

	| response |
	self setUpContextForPrinting.

	response := debugger parsesAndExecuteCommand: 'print 1/0'.

	self assert: debugger isActive.
	self assert: response equals: 'Error evaluating: 1/0'
]

{ #category : 'tests' }
CommandLineDebuggerBasicCommandsTest >> testPrintCommandWithNoArgumentsShowsNothingToPrint [

	| response |
	self setUpContextForPrinting.

	response := debugger parsesAndExecuteCommand: 'print '.

	self assert: response equals: 'Nothing to print'
]

{ #category : 'tests' }
CommandLineDebuggerBasicCommandsTest >> testPrintCommandWithSyntaxErrorReturnsError [

	| response |
	self setUpContextForPrinting.

	response := debugger parsesAndExecuteCommand: 'print 1 + '.

	self assert: debugger isActive.

	self assert: response equals: 'Error evaluating: 1 +'
]

{ #category : 'tests' }
CommandLineDebuggerBasicCommandsTest >> testPrintCommandWithUnknownVariableReturnsError [

    | response |
    self setUpContextForPrinting.
    
    response := debugger parsesAndExecuteCommand: 'print arg'.

    self assert: debugger isActive.
    self assert: response equals: 'Error evaluating: arg'
]

{ #category : 'tests' }
CommandLineDebuggerBasicCommandsTest >> testPrintStackCommandShowsCurrentContext [

	| response |
	response := debugger parsesAndExecuteCommand: 'printStack'.

	self assert: (response includesSubstring: debugger context printString)
]

{ #category : 'tests' }
CommandLineDebuggerBasicCommandsTest >> testPrintStackUpdatesCorrectlyOnMethodEntryAfterStep [

	| stackBefore stackInside |
	self setUpDebuggerOnNavigationMock.

	stackBefore := debugger parsesAndExecuteCommand: 'printStack'.

	debugger parsesAndExecuteCommand: 'step'. 
	stackInside := debugger parsesAndExecuteCommand: 'printStack'.

	self assert: (stackInside includesSubstring: 'methodToStepInto').
	self assert: (stackInside includesSubstring: 'methodForNavigationInACode').
	self deny: stackInside equals: stackBefore
]

{ #category : 'tests' }
CommandLineDebuggerBasicCommandsTest >> testPrintStackUpdatesCorrectlyOnMethodExitAfterStep [

	| stackInside stackAfter |
	self setUpDebuggerOnNavigationMock.

	debugger parsesAndExecuteCommand: 'step'.

	stackInside := debugger parsesAndExecuteCommand: 'printStack'.

	[ debugger context method selector = #methodToStepInto ] whileTrue: [ debugger parsesAndExecuteCommand: 'step' ]. "Get out of the method to get back on methodForNavigationInACode"

	stackAfter := debugger parsesAndExecuteCommand: 'printStack'.

	self assert: (stackAfter includesSubstring: 'methodForNavigationInACode').
	self deny: (stackAfter includesSubstring: 'methodToStepInto').
	self deny: stackInside equals: stackAfter
]

{ #category : 'tests' }
CommandLineDebuggerBasicCommandsTest >> testQuitDisactivateTheDebuggerAndAnnouncesIt [

	| response |
	self assert: debugger isActive.
	response := debugger parsesAndExecuteCommand: 'q'.
	self deny: debugger isActive.
	self assert: response equals: 'Debugging finished'
]

{ #category : 'tests' }
CommandLineDebuggerBasicCommandsTest >> testStepCommandEntersCalledMethod [

	| initialContext newContext |

	initialContext := self setUpDebuggerOnNavigationMock.
	debugger parsesAndExecuteCommand: 'step'.
	newContext := debugger context method.
	self deny: newContext method equals: initialContext method.
	self assert: newContext method selector equals: #methodToStepInto.
]

{ #category : 'tests' }
CommandLineDebuggerBasicCommandsTest >> testUnknownCommandReturnsErrorMessage [

	| response unknownCommand |
	unknownCommand := 'thisIsAnUnknownCommand'.
	response := debugger parsesAndExecuteCommand: unknownCommand.

	self assert: debugger isActive.
	self assert: response equals: 'Unknown command: ' , unknownCommand
]
