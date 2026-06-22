Class {
	#name : 'ClapDbgBasicCommandsTest',
	#superclass : 'TestCase',
	#instVars : [
		'debugger'
	],
	#category : 'Debugger-CLI-Tests',
	#package : 'Debugger-CLI-Tests'
}

{ #category : 'running' }
ClapDbgBasicCommandsTest >> setUp [

	| debuggingProcess debuggingContext debuggingSession |
	super setUp.
	debuggingProcess := [ self class compiler evaluate: 'OCParser parseMethod: foo self' ] newProcess.
	debuggingContext := debuggingProcess suspendedContext.
	debuggingSession := DebugSession named: 'test' on: debuggingProcess startedAt: debuggingContext.
	debugger := ClapDebugger new
		            context: debuggingContext;
		            process: debuggingProcess;
		            session: debuggingSession
]

{ #category : 'tests' }
ClapDbgBasicCommandsTest >> setUpContextForPrintCode [

	| currentContext |
	currentContext := Context
		                  sender: nil
		                  receiver: ClapDbgMockForDebugger new
		                  method: ClapDbgMockForDebugger >> #methodeToPrintCode
		                  arguments: #(  ).
	debugger context: currentContext
]

{ #category : 'running' }
ClapDbgBasicCommandsTest >> setUpContextForPrinting [

	| currentContext |
	currentContext := Context
		                  sender: nil
		                  receiver: ClapDbgMockForDebugger new
		                  method: ClapDbgMockForDebugger >> #methodToPrintCode:withArgument:
		                  arguments: #( 1 32 ).
	debugger context: currentContext
]

{ #category : 'running' }
ClapDbgBasicCommandsTest >> setUpDebuggerOnNavigationMock [

	| process context session |
	process := [ ClapDbgMockForDebugger new methodForNavigationInACode ] newProcess.
	context := process suspendedContext.

	session := DebugSession named: 'TestNav' on: process startedAt: context.

	debugger
		process: process;
		session: session;
		context: context.

	[ debugger context method selector = #methodForNavigationInACode ] whileFalse: [ debugger parsesAndExecuteCommand: 'step' ].
	^ debugger context
]

{ #category : 'running' }
ClapDbgBasicCommandsTest >> setUpDebuggerOnStepThroughMock [

	| process context session |
	process := [ ClapDbgMockForDebugger new methodWithBlockForStepThrough ] newProcess.
	context := process suspendedContext.  

	session := DebugSession named: 'TestStepThrough' on: process startedAt: context.

	debugger
		process: process;
		session: session;
		context: context.

	[ debugger context method selector = #methodWithBlockForStepThrough ] whileFalse: [ debugger parsesAndExecuteCommand: 'step' ].
	debugger parsesAndExecuteCommand: 'step'.

	^ debugger context
]

{ #category : 'tests' }
ClapDbgBasicCommandsTest >> testEmptyCommandDoesNothing [

	| response |
	response := debugger parsesAndExecuteCommand: '          '.

	self assert: response equals: ''
]

{ #category : 'tests' }
ClapDbgBasicCommandsTest >> testOverCommandAdvancesPCOnSameContext [
	
	| initialContext initialPc |

	initialContext := self setUpDebuggerOnNavigationMock.
	initialPc := initialContext pc.
	debugger parsesAndExecuteCommand: 'over'.
	self assert: debugger context method equals: initialContext method.
	self deny: debugger context pc equals: initialPc.
]

{ #category : 'tests' }
ClapDbgBasicCommandsTest >> testPrintCodeCommandShowsMethodSourceAndHighlightsCurrentInstruction [

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
ClapDbgBasicCommandsTest >> testPrintCommandEvaluatesArgumentsCorrectly [

	| response1 response2 |
	self setUpContextForPrinting.

	response1 := debugger parsesAndExecuteCommand: 'print arg1'.
	self assert: response1 equals: 'arg1 : 1'.

	response2 := debugger parsesAndExecuteCommand: 'print arg2'.
	self assert: response2 equals: 'arg2 : 32'
]

{ #category : 'tests' }
ClapDbgBasicCommandsTest >> testPrintCommandWithDivisionByZeroReturnsError [

	| response |
	self setUpContextForPrinting.

	response := debugger parsesAndExecuteCommand: 'print 1/0'.

	self assert: debugger isActive.
	self assert: (response includesSubstring: 'ZeroDivide')
]

{ #category : 'tests' }
ClapDbgBasicCommandsTest >> testPrintCommandWithNoArgumentsShowsNothingToPrint [

	| response |
	self setUpContextForPrinting.

	response := debugger parsesAndExecuteCommand: 'print '.

	self assert: response equals: 'Nothing to print'
]

{ #category : 'tests' }
ClapDbgBasicCommandsTest >> testPrintCommandWithSyntaxErrorReturnsError [

	| response |
	self setUpContextForPrinting.

	response := debugger parsesAndExecuteCommand: 'print 1 + '.

	self assert: debugger isActive.
	self assert: (response includesSubstring: 'OCSyntaxErrorNotice')
]

{ #category : 'tests' }
ClapDbgBasicCommandsTest >> testPrintCommandWithUnknownVariableReturnsError [

	| response |
	self setUpContextForPrinting.

	response := debugger parsesAndExecuteCommand: 'print arg'.

	self assert: debugger isActive.
	self assert: (response includesSubstring: 'OCUndeclaredVariableNotice')
]

{ #category : 'tests' }
ClapDbgBasicCommandsTest >> testPrintStackCommandShowsCurrentContext [

	| response |
	response := debugger parsesAndExecuteCommand: 'printStack'.

	self assert: (response includesSubstring: debugger context printString)
]

{ #category : 'tests' }
ClapDbgBasicCommandsTest >> testPrintStackUpdatesCorrectlyOnMethodEntryAfterStep [

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
ClapDbgBasicCommandsTest >> testPrintStackUpdatesCorrectlyOnMethodExitAfterStep [

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
ClapDbgBasicCommandsTest >> testQuitDisactivateTheDebuggerAndAnnouncesIt [

	| response |
	self assert: debugger isActive.
	response := debugger parsesAndExecuteCommand: 'q'.
	self deny: debugger isActive.
	self assert: response equals: 'Debugging finished'
]

{ #category : 'tests' }
ClapDbgBasicCommandsTest >> testStepCommandEntersCalledMethod [

	| initialContext newContext |

	initialContext := self setUpDebuggerOnNavigationMock.
	debugger parsesAndExecuteCommand: 'step'.
	newContext := debugger context method.
	self deny: newContext method equals: initialContext method.
	self assert: newContext method selector equals: #methodToStepInto.
]

{ #category : 'tests' }
ClapDbgBasicCommandsTest >> testStepThroughCommandEntersBlock [

	| initialContext response codeBefore |
	initialContext := self setUpDebuggerOnStepThroughMock.
	codeBefore := debugger parsesAndExecuteCommand: 'pc'.

	response := debugger parsesAndExecuteCommand: 'stepThrough'.
	self assert: debugger context isBlockContext.

	self assert: debugger context home method selector equals: #methodWithBlockForStepThrough
]

{ #category : 'tests' }
ClapDbgBasicCommandsTest >> testUnknownCommandReturnsErrorMessage [

	| response unknownCommand |
	unknownCommand := 'thisIsAnUnknownCommand'.
	response := debugger parsesAndExecuteCommand: unknownCommand.

	self assert: debugger isActive.
	self assert: response equals: 'Unknown command: ' , unknownCommand
]
