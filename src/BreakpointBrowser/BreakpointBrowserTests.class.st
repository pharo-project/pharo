Class {
	#name : #BreakpointBrowserTests,
	#superclass : #TestCase,
	#instVars : [
		'breakpointsAddedDuringSetupAndTests'
	],
	#category : #BreakpointBrowser
}

{ #category : #adding }
BreakpointBrowserTests >> addBreakpointConditionalOnSetupHere [
"Dummy method to indicate a position to put a conditional breakpoint at in the setUp of this test class"
]

{ #category : #adding }
BreakpointBrowserTests >> addBreakpointOnSetupHere [
"Dummy method to indicate a position to put a breakpoint at in the setUp of this test class"
]

{ #category : #adding }
BreakpointBrowserTests >> addBreakpointOnceOnSetupHere [
"Dummy method to indicate a position to put a once breakpoint at in the setUp of this test class"
]

{ #category : #'halts and breakpoints' }
BreakpointBrowserTests >> methodWithBreakpoint [
	self addBreakpointOnSetupHere "Breakpoint will be added during setup and removed during teardown"
]

{ #category : #'halts and breakpoints' }
BreakpointBrowserTests >> methodWithBreakpointConditional [
	self addBreakpointConditionalOnSetupHere "Breakpoint will be added during setup and removed during teardown"
]

{ #category : #'halts and breakpoints' }
BreakpointBrowserTests >> methodWithBreakpointOnce [
	self addBreakpointOnceOnSetupHere "Breakpoint will be added during setup and removed during teardown"
]

{ #category : #'halts and breakpoints' }
BreakpointBrowserTests >> methodWithHalt [
	self halt
]

{ #category : #'halts and breakpoints' }
BreakpointBrowserTests >> methodWithHaltColon [
	self halt: 'Hey'
]

{ #category : #'halts and breakpoints' }
BreakpointBrowserTests >> methodWithHaltIf [
	self haltIf: [ true ]
]

{ #category : #'halts and breakpoints' }
BreakpointBrowserTests >> methodWithHaltIfNil [
	self haltIfNil
]

{ #category : #'halts and breakpoints' }
BreakpointBrowserTests >> methodWithHaltOnCount [
	self haltOnCount: 4
]

{ #category : #'halts and breakpoints' }
BreakpointBrowserTests >> methodWithHaltOnce [
	self haltOnce
]

{ #category : #'halts and breakpoints' }
BreakpointBrowserTests >> methodWithHaltSentToOne [
	1halt
]

{ #category : #running }
BreakpointBrowserTests >> setUp [
	breakpointsAddedDuringSetupAndTests := LinkedList new.

	"Adding breakpoints to the message node of selector addBreakpointOnSetupHere in method methodWithBreakpoint"
	(MessageNodeFinder findMessageNodesSatisfying: [ :msgNode | msgNode selector == #addBreakpointOnSetupHere ] inNode: (self class >>#methodWithBreakpoint) ast) do: [
		 :node |
		|brkpt| 
		brkpt := Breakpoint new node: node; always; install.
		breakpointsAddedDuringSetupAndTests add: brkpt.
	].

	"Adding once breakpoints to the message node of selector addBreakpointOnceOnSetupHere in method methodWithBreakpointOnce"
	(MessageNodeFinder findMessageNodesSatisfying: [ :msgNode | msgNode selector == #addBreakpointOnceOnSetupHere ] inNode: (self class >>#methodWithBreakpointOnce) ast) do: [
		 :node |
		|brkpt| 
		brkpt := Breakpoint new node: node; once; install.
		breakpointsAddedDuringSetupAndTests add: brkpt.
	].

	"Adding conditional breakpoints to the message node of selector addBreakpointConditionalOnSetupHere in method methodWithBreakpointConditional"
	(MessageNodeFinder findMessageNodesSatisfying: [ :msgNode | msgNode selector == #addBreakpointConditionalOnSetupHere ] inNode: (self class >>#methodWithBreakpointConditional) ast) do: [
		 :node |
		|brkpt| 
		brkpt := Breakpoint new node: node; condition: (Smalltalk compiler evaluate: 'true'); install.
		breakpointsAddedDuringSetupAndTests add: brkpt.
	].
]

{ #category : #tests }
BreakpointBrowserTests >> testMethodsToDisplay [
	| methodsToDisplay methodContainmentAssertionBlock |
	methodsToDisplay := BreakpointBrowser methodsToDisplay collect: [:anRGMethodDefinition| anRGMethodDefinition method].
	methodContainmentAssertionBlock := [ :selector | 	self assert: (methodsToDisplay contains: [:aCompiledMethod | aCompiledMethod == (self class >>selector)]) ].

	methodContainmentAssertionBlock value: #methodWithBreakpoint.
	methodContainmentAssertionBlock value: #methodWithBreakpointConditional.
	methodContainmentAssertionBlock value: #methodWithBreakpointOnce.
	methodContainmentAssertionBlock value: #methodWithHalt.
	methodContainmentAssertionBlock value: #methodWithHaltColon.
	methodContainmentAssertionBlock value: #methodWithHaltIf.
	methodContainmentAssertionBlock value: #methodWithHaltIfNil.
	methodContainmentAssertionBlock value: #methodWithHaltOnCount.
	methodContainmentAssertionBlock value: #methodWithHaltOnce.
	methodContainmentAssertionBlock value: #methodWithHaltSentToOne.
]
