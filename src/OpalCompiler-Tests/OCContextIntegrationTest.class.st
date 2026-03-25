"
I am an SUnit Test of Context.

See pages 430-437 of A. Goldberg and D. Robson's  Smalltalk-80 The Language (aka the purple book), which deal with Contexts. My fixtures are from their example. (The Squeak byte codes are not quite the same as Smalltalk-80.)
My fixtures are:
aReceiver         - just some arbitrary object, ""Rectangle origin: 100@100 corner: 200@200""
aSender           - just some arbitrary object, thisContext
aCompiledMethod - just some arbitrary method, ""Rectangle rightCenter"".
aMethodContext   - just some arbitray context ...  
nonActiveBlockContext  - the Context of a block whose home context is not in its sender chain (because the method that created this block already returned)

"
Class {
	#name : 'OCContextIntegrationTest',
	#superclass : 'TestCase',
	#instVars : [
		'aCompiledMethod',
		'aReceiver',
		'aSender',
		'anArgument',
		'aMethodContext'
	],
	#category : 'OpalCompiler-Tests-Runtime',
	#package : 'OpalCompiler-Tests',
	#tag : 'Runtime'
}

{ #category : 'running' }
OCContextIntegrationTest >> setUp [

	super setUp.
	aCompiledMethod := Rectangle compiledMethodAt: #areasOutside:.
	aReceiver := 100 @ 100 corner: 200 @ 200.
	aSender := thisContext.
	anArgument := 420 @ 420 corner: 200 @ 200.
	aMethodContext := Context
		                  sender: aSender
		                  receiver: aReceiver
		                  method: aCompiledMethod
		                  arguments: { anArgument }
]

{ #category : 'tests' }
OCContextIntegrationTest >> testAstScope [

	| scope |
	self assert: aMethodContext compiledCode hasSourceCode.
	
	"test that we get a scope for optimized blocks"
	aMethodContext pc: 88 + 1.
	scope := aMethodContext astScope.
	self assert: scope isOptimized.

	"test that we do not get a block scope when pushing a block on the stack"
	aMethodContext pc: 95 + 1.
	scope := aMethodContext astScope.
	self assert: aMethodContext sourceNodeExecuted isBlock.
	self deny: scope isBlockScope
]
