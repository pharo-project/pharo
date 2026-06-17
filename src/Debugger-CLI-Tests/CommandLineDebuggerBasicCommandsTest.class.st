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
CommandLineDebuggerBasicCommandsTest >> testEmptyCommand [

	| response |
	response := debugger parsesAndExecuteCommand: '          '.

	self assert: response equals: ''
]

{ #category : 'tests' }
CommandLineDebuggerBasicCommandsTest >> testOver [
	"testing that the Program Counter changed after executing step"

	| response initialPc |
	initialPc := debugger context pc.

	response := debugger parsesAndExecuteCommand: 'over'.

	self deny: debugger context pc equals: initialPc.

	self assert: debugger isActive.

	self assert: (response beginsWith: 'Stepped OVER to:')
]

{ #category : 'tests' }
CommandLineDebuggerBasicCommandsTest >> testPrint [

	| currentContext response1 response2 |
	currentContext := Context
		                  sender: nil
		                  receiver: self
		                  method: MockToPrintCode >> #mockMethodForTesting:and:
		                  arguments: #( 1 32 ).
	debugger context: currentContext.

	response1 := debugger parsesAndExecuteCommand: 'print arg1'.

	self assert: response1 equals: 'arg1 : 1'.

	response2 := debugger parsesAndExecuteCommand: 'print arg2'.

	self assert: response2 equals: 'arg2 : 32'
]

{ #category : 'tests' }
CommandLineDebuggerBasicCommandsTest >> testPrintCode [

	| currentContext response |
	currentContext := Context
		                  sender: nil
		                  receiver: self
		                  method: MockToPrintCode >> #mockMethodForTesting
		                  arguments: #(  ).
	debugger context: currentContext.

	response := debugger parsesAndExecuteCommand: 'printCode'.


	self assert: (response includesSubstring: 'mockMethodForTesting').
	self assert: (response includesSubstring: '"Fake method to test printCode"').
	self assert: (response includesSubstring: 'a := --->42<---.').
	self assert: (response includesSubstring: 'b := a + 1.').
	self assert: (response includesSubstring: '^ b * 2').

	self assert: debugger isActive
]

{ #category : 'tests' }
CommandLineDebuggerBasicCommandsTest >> testPrintError [

	| currentContext response |
	currentContext := Context
		                  sender: nil
		                  receiver: self
		                  method: MockToPrintCode >> #mockMethodForTesting:and:
		                  arguments: #( 1 32 ).
	debugger context: currentContext.

	response := debugger parsesAndExecuteCommand: 'print arg'.

	self assert: debugger isActive.
	self assert: response equals: 'Error evaluating: arg'.


	response := debugger parsesAndExecuteCommand: 'print 1/0'.

	self assert: debugger isActive.
	self assert: response equals: 'Error evaluating: 1/0'
]

{ #category : 'tests' }
CommandLineDebuggerBasicCommandsTest >> testPrintStack [

	| response initialPc |
	initialPc := debugger context pc.
	response := debugger parsesAndExecuteCommand: 'printStack'.

	self assert: (response includesSubstring: debugger context printString)
]

{ #category : 'tests' }
CommandLineDebuggerBasicCommandsTest >> testPrintStackAfterStep [

	| response contextAfterStep |
	debugger parsesAndExecuteCommand: 'step'.
	contextAfterStep := debugger context.

	response := debugger parsesAndExecuteCommand: 'printStack'.

	self assert: (response includesSubstring: contextAfterStep printString)
]

{ #category : 'tests' }
CommandLineDebuggerBasicCommandsTest >> testPrintWithoutArguments [

	| currentContext response |
	currentContext := Context
		                  sender: nil
		                  receiver: self
		                  method: MockToPrintCode >> #mockMethodForTesting:and:
		                  arguments: #( 1 32 ).
	debugger context: currentContext.

	response := debugger parsesAndExecuteCommand: 'print '.

	self assert: response equals: 'Nothing to print'
]

{ #category : 'tests' }
CommandLineDebuggerBasicCommandsTest >> testQuit [

	| response |
	self assert: debugger isActive.
	response := debugger parsesAndExecuteCommand: 'q'.
	self deny: debugger isActive.
	self assert: response equals: 'Debugging finished'
]

{ #category : 'tests' }
CommandLineDebuggerBasicCommandsTest >> testStep [
	"testing that the Program Counter changed after executing step"

	| response initialPc |
	initialPc := debugger context pc.

	response := debugger parsesAndExecuteCommand: 'step'.

	self deny: debugger context pc equals: initialPc.

	self assert: debugger isActive.

	self assert: (response beginsWith: 'Stepped to:')
]

{ #category : 'tests' }
CommandLineDebuggerBasicCommandsTest >> testUnknownCommand [

	| response unknownCommand |
	unknownCommand := 'thisIsAnUnknownCommand'.
	response := debugger parsesAndExecuteCommand: unknownCommand.

	self assert: debugger isActive.
	self assert: response equals: 'Unknown command: ' , unknownCommand
]
