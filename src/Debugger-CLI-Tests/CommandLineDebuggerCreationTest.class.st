Class {
	#name : 'CommandLineDebuggerCreationTest',
	#superclass : 'TestCase',
	#category : 'Debugger-CLI-Tests',
	#package : 'Debugger-CLI-Tests'
}

{ #category : 'tests' }
CommandLineDebuggerCreationTest >> testForCodeCreation [

	| newDebugger |
	newDebugger := CommandLineDebugger forCode: '3 + 4'.

	self assert: newDebugger process isNotNil.
	self assert: newDebugger process isSuspended.
	self assert: newDebugger context isNotNil.
	self assert: newDebugger session isNotNil
]

{ #category : 'tests' }
CommandLineDebuggerCreationTest >> testForCodeCreationWithSyntaxError [

	self should: [ CommandLineDebugger forCode: '1 + ' ] raise: Error 
]

{ #category : 'tests' }
CommandLineDebuggerCreationTest >> testForProcessCreation [

	| blockToDebug testProcess newDebugger |
	blockToDebug := [ 1 + 1 ].
	testProcess := blockToDebug newProcess.

	newDebugger := CommandLineDebugger forProcess: testProcess.

	self assert: newDebugger process equals: testProcess.
	self assert: newDebugger context equals: testProcess suspendedContext.
	self assert: newDebugger session isNotNil.
	self assert: newDebugger isActive
]

{ #category : 'tests' }
CommandLineDebuggerCreationTest >> testInStreamAndOutStreamAtCreation [

	| newDebugger fakeInStream fakeOutStream result |
	newDebugger := CommandLineDebugger new.
	fakeInStream := ReadStream on: 'test'.
	fakeOutStream := String new writeStream. 
	result := newDebugger
		          inStream: fakeInStream;
		          outStream: fakeOutStream. 
	self assert: newDebugger inStream equals: fakeInStream.
	self assert: newDebugger outStream equals: fakeOutStream. 
]
