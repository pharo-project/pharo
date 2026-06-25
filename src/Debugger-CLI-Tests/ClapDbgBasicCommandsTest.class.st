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

	[ debugger context method selector = #methodForNavigationInACode ] whileFalse: [ debugger step ].
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

	[ debugger context method selector = #methodWithBlockForStepThrough ] whileFalse: [ debugger step ].
	debugger step.

	^ debugger context
]

{ #category : 'tests' }
ClapDbgBasicCommandsTest >> testDownCommandAtBottomOfTheStackDoesNothing [

	| response bottomContextInTheStack |
	self setUpDebuggerOnNavigationMock.

	bottomContextInTheStack := debugger context bottomContext.
	debugger context: bottomContextInTheStack.
	response := debugger down.

	self assert: debugger context equals: bottomContextInTheStack.
	self assert: response equals: 'Already at the bottom of the stack'
]

{ #category : 'tests' }
ClapDbgBasicCommandsTest >> testDownCommandGoesDownInTheStackWhenNotAtTheBottom [
    | topContext downContext |
    topContext := self setUpDebuggerOnNavigationMock.
    
    debugger down.
    downContext := debugger context.
    
    self deny: downContext equals: topContext.
    self assert: downContext equals: topContext sender.
    
]

{ #category : 'tests' }
ClapDbgBasicCommandsTest >> testFinishCommandExitsMethodAndPreservesReturnExecution [
    | contextBeforeEntering |
    self setUpDebuggerOnNavigationMock.
    contextBeforeEntering := debugger context.
    debugger step. "to enter the method"

    debugger finish.
    
    self assert: debugger context equals: contextBeforeEntering.
    
    debugger step.

    self assert: (debugger print: 'response') equals: '42'
]

{ #category : 'tests' }
ClapDbgBasicCommandsTest >> testOverCommandAdvancesPCOnSameContext [
	
	| initialContext initialPc |

	initialContext := self setUpDebuggerOnNavigationMock.
	initialPc := initialContext pc.
	debugger over.
	self assert: debugger context method equals: initialContext method.
	self deny: debugger context pc equals: initialPc.
]

{ #category : 'tests' }
ClapDbgBasicCommandsTest >> testParsingAnEmptyCommandDoesNothing [

	| response |
	response := debugger parsesAndExecuteCommand: '          '.

	self assert: response equals: ''
]

{ #category : 'tests' }
ClapDbgBasicCommandsTest >> testPrintCodeCommandShowsMethodSourceAndHighlightsNextInstruction [

    | response |
    self setUpContextForPrintCode.
    
    response := debugger printCode.

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

	response1 := debugger print: 'arg1'.
	self assert: response1 equals: '1'.

	response2 := debugger print: 'arg2'.
	self assert: response2 equals: '32'
]

{ #category : 'tests' }
ClapDbgBasicCommandsTest >> testPrintCommandWithDivisionByZeroReturnsError [

	| response |
	self setUpContextForPrinting.

	response := debugger print: '1/0'.

	self assert: debugger isActive.
	self assert: (response includesSubstring: 'ZeroDivide')
]

{ #category : 'tests' }
ClapDbgBasicCommandsTest >> testPrintCommandWithNoArgumentsShowsNothingToPrint [

	| response |
	self setUpContextForPrinting.

	response := debugger print: ' '.

	self assert: response equals: 'Nothing to print'
]

{ #category : 'tests' }
ClapDbgBasicCommandsTest >> testPrintCommandWithSyntaxErrorReturnsError [

	| response |
	self setUpContextForPrinting.

	response := debugger print: '1+ '.

	self assert: debugger isActive.
	self assert: (response includesSubstring: 'OCSyntaxErrorNotice')
]

{ #category : 'tests' }
ClapDbgBasicCommandsTest >> testPrintCommandWithUnknownVariableReturnsError [

	| response |
	self setUpContextForPrinting.

	response := debugger print: 'arg'.

	self assert: debugger isActive.
	self assert: (response includesSubstring: 'OCUndeclaredVariableNotice')
]

{ #category : 'tests' }
ClapDbgBasicCommandsTest >> testPrintStackCommandShowsCurrentContext [

	| response |
	response := debugger printStack.

	self assert: (response includesSubstring: debugger context printString)
]

{ #category : 'tests' }
ClapDbgBasicCommandsTest >> testPrintStackUpdatesCorrectlyOnMethodEntryAfterStep [

	| stackBefore stackInside |
	self setUpDebuggerOnNavigationMock.

	stackBefore := debugger printStack.

	debugger step. 
	stackInside := debugger printStack.

	self assert: (stackInside includesSubstring: 'methodToStepInto').
	self assert: (stackInside includesSubstring: 'methodForNavigationInACode').
	self deny: stackInside equals: stackBefore
]

{ #category : 'tests' }
ClapDbgBasicCommandsTest >> testPrintStackUpdatesCorrectlyOnMethodExitAfterStep [

	| stackInside stackAfter |
	self setUpDebuggerOnNavigationMock.

	debugger step.

	stackInside := debugger printStack.

	[ debugger context method selector = #methodToStepInto ] whileTrue: [ debugger step ]. "Get out of the method to get back on methodForNavigationInACode"

	stackAfter := debugger printStack.

	self assert: (stackAfter includesSubstring: 'methodForNavigationInACode').
	self deny: (stackAfter includesSubstring: 'methodToStepInto').
	self deny: stackInside equals: stackAfter
]

{ #category : 'tests' }
ClapDbgBasicCommandsTest >> testQuitDisactivateTheDebuggerAndAnnouncesIt [

	| response |
	self assert: debugger isActive.
	response := debugger quit.
	self deny: debugger isActive.
	self assert: response equals: 'Debugging finished'
]

{ #category : 'tests' }
ClapDbgBasicCommandsTest >> testReturnExitTheFunctionReturningTheArgument [

	| contextBeforeEntering |
	self setUpDebuggerOnNavigationMock.
	contextBeforeEntering := debugger context.

	debugger step. "to enter the method methodToStepInto"
	debugger return: '12'.

	self assert: debugger context equals: contextBeforeEntering.
	debugger step. "to assign the return value to response"
	self assert: (debugger print: 'response') equals: '12'
]

{ #category : 'tests' }
ClapDbgBasicCommandsTest >> testReturnWithInvalidExpressionReturnsEvaluationErrorWithoutReturning [

	| response initialContext |
	initialContext := self setUpDebuggerOnNavigationMock.

	response := debugger return: '1/0'.

	self assert: debugger context equals: initialContext.
	self assert: (response includesSubstring: 'ZeroDivide' caseSensitive: false)
]

{ #category : 'tests' }
ClapDbgBasicCommandsTest >> testStepCommandEntersCalledMethod [

	| initialContext newContext |

	initialContext := self setUpDebuggerOnNavigationMock.
	debugger step.
	newContext := debugger context method.
	self deny: newContext method equals: initialContext method.
	self assert: newContext method selector equals: #methodToStepInto.
]

{ #category : 'tests' }
ClapDbgBasicCommandsTest >> testStepThroughCommandEntersBlock [

	| initialContext response |
	initialContext := self setUpDebuggerOnStepThroughMock.
	response := debugger stepThrough.

	self assert: debugger context isBlockContext.
	self assert: debugger context home method selector equals: #methodWithBlockForStepThrough
]

{ #category : 'tests' }
ClapDbgBasicCommandsTest >> testUnknownCommandReturnsErrorMessage [

	| response unknownCommand |
	unknownCommand := 'thisIsAnUnknownCommand'.
	response := debugger executeCommand: unknownCommand arg: ''.

	self assert: debugger isActive.
	self assert: response equals: 'Unknown command: ' , unknownCommand
]

{ #category : 'tests' }
ClapDbgBasicCommandsTest >> testUpCommandAfterDownCommandGoesBackToRecentContext [

	| topContext |
	topContext := self setUpDebuggerOnNavigationMock.

	debugger down.

	debugger up.

	self assert: debugger context equals: topContext
]

{ #category : 'tests' }
ClapDbgBasicCommandsTest >> testUpCommandAtTopOfTheStackDoesNothing [

	| initialContext response |
	initialContext := self setUpDebuggerOnNavigationMock.
	response := debugger up.

	self assert: debugger context equals: initialContext.

	self assert: response equals: 'Already at the top of the stack'
]
