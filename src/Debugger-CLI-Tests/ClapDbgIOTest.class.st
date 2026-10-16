Class {
	#name : 'ClapDbgIOTest',
	#superclass : 'TestCase',
	#instVars : [
		'debugger'
	],
	#category : 'Debugger-CLI-Tests',
	#package : 'Debugger-CLI-Tests'
}

{ #category : 'running' }
ClapDbgIOTest >> setUp [

	super setUp.
	debugger := ClapDebugger new.

	debugger lineEnding: String lf
]

{ #category : 'running' }
ClapDbgIOTest >> testAskTypingWritesToOutStream [

	| tempStream |
	tempStream := String new writeStream. 
	debugger outStream: tempStream.

	debugger askTyping. 
	self assert: tempStream contents equals: '(debug) '
]

{ #category : 'running' }
ClapDbgIOTest >> testInformResultsWritesToOutStream [

	| tempWriteStream |
	tempWriteStream := String new writeStream.
	debugger outStream: tempWriteStream.
	debugger printOutput: 'Hello World'.
	self assert: tempWriteStream contents equals: 'Hello World' , String lf
]

{ #category : 'running' }
ClapDbgIOTest >> testReadNextLine [

	| tempReadStream |
	tempReadStream := ReadStream on: 'step' , String lf , 'p a' , String lf.
	debugger inStream: tempReadStream.

	self assert: debugger readNextLine equals: 'step'.
	self assert: debugger readNextLine equals: 'p a'
]

{ #category : 'running' }
ClapDbgIOTest >> testRunLoopSimulation [

	| tempReadStream tempWriteStream |
	tempReadStream := ReadStream on: 'help' , String lf , 'quit' , String lf.
	tempWriteStream := String new writeStream.

	debugger inStream: tempReadStream.
	debugger outStream: tempWriteStream.
	debugger run.

	self deny: debugger isActive.
	self assert: (tempWriteStream contents includesSubstring: '(debug) ').
	self assert: (tempWriteStream contents includesSubstring: 'Debugging finished')
]
