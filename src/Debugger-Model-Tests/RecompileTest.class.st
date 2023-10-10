Class {
	#name : 'RecompileTest',
	#superclass : 'DebuggerTest',
	#category : 'Debugger-Model-Tests-Core',
	#package : 'Debugger-Model-Tests',
	#tag : 'Core'
}

{ #category : 'tests' }
RecompileTest >> testRecompileDoIt [

	| method node |
	method := nil compiler
		          isScripting: true;
		          compile: '| a | a := 42'.
	self settingUpSessionAndProcessAndContextForBlock: [
		nil executeMethod: method ].
	[ session interruptedContext compiledCode = method ] whileFalse: [
		session stepInto ].

	node := method sourceNodeForPC: session interruptedContext pc.
	self assert: node isAssignment.
	self assert: node variable name equals: #a.

	session stepOver.
	self assert: (session interruptedContext tempNamed: #a) equals: 42.

	session
		recompileMethodTo: '|c| c := 43'
		inContext: session interruptedContext
		notifying: nil.
	method := session interruptedContext method.

	node := method sourceNodeForPC: session interruptedContext pc.
	self assert: node isAssignment.
	self assert: node variable name equals: #c.
	self assert: (session interruptedContext tempNamed: #c) equals: nil.

	session stepOver.
	self assert: (session interruptedContext tempNamed: #c) equals: 43
]

{ #category : 'tests' }
RecompileTest >> testRecompileInstalled [

	| method node |
	method := self class compiler install: 'foo | a | a := 42'.
	self
		assert: (self class >> #foo) sourceCode
		equals: 'foo | a | a := 42'.

	self settingUpSessionAndProcessAndContextForBlock: [
		nil executeMethod: method ].
	[ session interruptedContext compiledCode = method ] whileFalse: [
		session stepInto ].

	node := method sourceNodeForPC: session interruptedContext pc.
	self assert: node isAssignment.
	self assert: node variable name equals: #a.

	session stepOver.
	self assert: (session interruptedContext tempNamed: #a) equals: 42.

	session
		recompileMethodTo: 'foo | c | c := 43'
		inContext: session interruptedContext
		notifying: nil.
	method := session interruptedContext method.

	node := method sourceNodeForPC: session interruptedContext pc.
	self assert: node isAssignment.
	self assert: node variable name equals: #c.
	self assert: (session interruptedContext tempNamed: #c) equals: nil.

	session stepOver.
	self assert: (session interruptedContext tempNamed: #c) equals: 43.

	self
		assert: (self class >> #foo) sourceCode
		equals: 'foo | c | c := 43'.
	(self class >> #foo) removeFromSystem
]

{ #category : 'tests' }
RecompileTest >> testRecompileUninstalled [

	| method node |
	method := self class compiler compile: 'foo | a | a := 42'.
	self settingUpSessionAndProcessAndContextForBlock: [
		nil executeMethod: method ].
	[ session interruptedContext compiledCode = method ] whileFalse: [
		session stepInto ].

	node := method sourceNodeForPC: session interruptedContext pc.
	self assert: node isAssignment.
	self assert: node variable name equals: #a.

	session stepOver.
	self assert: (session interruptedContext tempNamed: #a) equals: 42.

	session
		recompileMethodTo: 'foo | c | c := 43'
		inContext: session interruptedContext
		notifying: nil.
	method := session interruptedContext method.

	node := method sourceNodeForPC: session interruptedContext pc.
	self assert: node isAssignment.
	self assert: node variable name equals: #c.
	self assert: (session interruptedContext tempNamed: #c) equals: nil.

	session stepOver.
	self assert: (session interruptedContext tempNamed: #c) equals: 43
]
