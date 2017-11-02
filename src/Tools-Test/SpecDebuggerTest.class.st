"
SUnit tests for SpecDebugger
"
Class {
	#name : #SpecDebuggerTest,
	#superclass : #TestCase,
	#category : 'Tools-Test-Debugger'
}

{ #category : #testing }
SpecDebuggerTest >> testBasic [
	| context process debugger printedString |
	context := [ 20 factorial ] asContext.
	process := Process
		forContext: context
		priority: Processor userInterruptPriority.
	debugger := SpecDebugger on: (process newDebugSessionNamed: 'test debugging' startedAt: context).
	debugger stack expand.
	self assert: debugger stack selectedIndex equals: 1.
	printedString := OpalCompiler isActive
		ifTrue: [ '[ 20 factorial ] in SpecDebuggerTest>>testBasic' ]
		ifFalse: [ '[...] in SpecDebuggerTest>>testBasic' ].
	self assert: debugger stack selectedItem printString equals: printedString.
	debugger send.
	debugger send.
	self
		assert: debugger code getText equals: (Integer >> #factorial) sourceCode.
	self
		assert:
			debugger stack selectedItem printString
		equals: 'SmallInteger(Integer)>>factorial'.
	process terminate
]
