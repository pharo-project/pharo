Class {
	#name : 'ClapDbgCreationTest',
	#superclass : 'TestCase',
	#category : 'Debugger-CLI-Tests',
	#package : 'Debugger-CLI-Tests'
}

{ #category : 'tests' }
ClapDbgCreationTest >> testForCodeCreation [

	| newDebugger |
	newDebugger := ClapDebugger forCode: '3 + 4'.

	self assert: newDebugger process isNotNil.
	self assert: newDebugger process isSuspended.
	self assert: newDebugger context isNotNil.
	self assert: newDebugger session isNotNil
]

{ #category : 'tests' }
ClapDbgCreationTest >> testForCodeCreationWithSyntaxError [

	self should: [ ClapDebugger forCode: '1 + ' ] raise: Error 
]

{ #category : 'tests' }
ClapDbgCreationTest >> testForProcessCreation [

	| blockToDebug testProcess newDebugger |
	blockToDebug := [ 1 + 1 ].
	testProcess := blockToDebug newProcess.

	newDebugger := ClapDebugger forProcess: testProcess.

	self assert: newDebugger process equals: testProcess.
	self assert: newDebugger context equals: testProcess suspendedContext.
	self assert: newDebugger session isNotNil.
	self assert: newDebugger isActive
]

{ #category : 'tests' }
ClapDbgCreationTest >> testInStreamAndOutStreamAtCreation [

	| newDebugger fakeInStream fakeOutStream result |
	newDebugger := ClapDebugger new.
	fakeInStream := ReadStream on: 'test'.
	fakeOutStream := String new writeStream. 
	result := newDebugger
		          inStream: fakeInStream;
		          outStream: fakeOutStream. 
	self assert: newDebugger inStream equals: fakeInStream.
	self assert: newDebugger outStream equals: fakeOutStream. 
]
