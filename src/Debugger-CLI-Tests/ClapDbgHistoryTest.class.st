Class {
	#name : 'ClapDbgHistoryTest',
	#superclass : 'TestCase',
	#instVars : [
		'debugger'
	],
	#category : 'Debugger-CLI-Tests',
	#package : 'Debugger-CLI-Tests'
}

{ #category : 'running' }
ClapDbgHistoryTest >> setUp [
    super setUp.
    debugger := ClapDebugger new. 

    debugger commandsStack push: 'print a'. 
    debugger commandsStack push: 'step'.     
    debugger commandsStack push: 'over'.   
    debugger commandsStack push: 'help'.    
]

{ #category : 'running' }
ClapDbgHistoryTest >> testHandleContainingStringHistory [

	self assert: (debugger handleHistory: '!?ste') equals: 'step'.
	self assert: (debugger handleHistory: '!?int') equals: 'print a'.
	self assert: (debugger handleHistory: '!?t') equals: 'step'.
	self assert: (debugger handleHistory: '!?e') equals: 'help'
]

{ #category : 'running' }
ClapDbgHistoryTest >> testHandleHistoryInvalidInputs [

	self
		should: [ debugger handleHistory: '!-9' ]
		raise: ClapDbgHistoryEventNotFound
		withExceptionDo: [ :ex | self assert: ex messageText equals: '!-9: event not found' ].
	self
		should: [ debugger handleHistory: '!9' ]
		raise: ClapDbgHistoryEventNotFound
		withExceptionDo: [ :ex | self assert: ex messageText equals: '!9: event not found' ].
	self
		should: [ debugger handleHistory: '!abc' ]
		raise: ClapDbgHistoryEventNotFound
		withExceptionDo: [ :ex | self assert: ex messageText equals: '!abc: event not found' ].
	self
		should: [ debugger handleHistory: '!?wontfind' ]
		raise: ClapDbgHistoryEventNotFound
		withExceptionDo: [ :ex | self assert: ex messageText equals: '!?wontfind: event not found' ].

	self assert: (debugger handleHistory: '!') equals: '!'
]

{ #category : 'running' }
ClapDbgHistoryTest >> testHandleHistoryLastCommand [

	self assert: (debugger handleHistory: '!!') equals: 'help'.
	debugger commandsStack push: 'help'.
	self assert: (debugger handleHistory: '!! arg1') equals: 'help arg1'.
	debugger commandsStack push: 'test'.
	self assert: (debugger handleHistory: '') equals: 'test'
]

{ #category : 'running' }
ClapDbgHistoryTest >> testHandleHistoryLeavesStandardCommandsIntact [

	self assert: (debugger handleHistory: 'step') equals: 'step'.
	self assert: (debugger handleHistory: 'p 10') equals: 'p 10'
]

{ #category : 'running' }
ClapDbgHistoryTest >> testHandleNumericHistoryNegative [

	self assert: (debugger handleHistory: '!-1') equals: 'help'.
	self assert: (debugger handleHistory: '!-2') equals: 'over'.
	self assert: (debugger handleHistory: '!-4') equals: 'print a'
]

{ #category : 'running' }
ClapDbgHistoryTest >> testHandleNumericHistoryPositive [

	self assert: (debugger handleHistory: '!1') equals: 'print a'.
	self assert: (debugger handleHistory: '!2') equals: 'step'.
	self assert: (debugger handleHistory: '!4') equals: 'help'
]

{ #category : 'running' }
ClapDbgHistoryTest >> testHandleStartingWithStringHistory [

	self assert: (debugger handleHistory: '!ste') equals: 'step'.
	self assert: (debugger handleHistory: '!pr') equals: 'print a'.
	self assert: (debugger handleHistory: '!h') equals: 'help'
]
