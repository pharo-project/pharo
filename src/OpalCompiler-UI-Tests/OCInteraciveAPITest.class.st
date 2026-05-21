Class {
	#name : 'OCInteraciveAPITest',
	#superclass : 'TestCase',
	#instVars : [
		'node',
		'text',
		'requestor'
	],
	#category : 'OpalCompiler-UI-Tests',
	#package : 'OpalCompiler-UI-Tests'
}

{ #category : 'interactive error protocol' }
OCInteraciveAPITest >> correctFrom: anInteger to: anInteger2 with: aString [ 

	text := text copyReplaceFrom: anInteger to: anInteger2 with: aString
]

{ #category : 'accessing' }
OCInteraciveAPITest >> node: aNode [

	node := aNode
]

{ #category : 'correcting' }
OCInteraciveAPITest >> repair [
	
	(OCCodeReparator new node: node)
		requestor: requestor;
		declareTempAndPaste: node name.
	^ true
]

{ #category : 'accessing' }
OCInteraciveAPITest >> requestor: anOCInteraciveAPITest [ 
	
	requestor := anOCInteraciveAPITest
]

{ #category : 'tests' }
OCInteraciveAPITest >> testUndeclaredsAreDefined [

	| method |
	text := 'foo "x and y are not declared" x + y'.
	method := OCInteractiveAPI new
		          undeclaredVariableReparator: self;
		          compile: text notifying: self in: self class.

	self assert: text equals: 'foo "x and y are not declared" | x y |
x + y'
]

{ #category : 'accessing' }
OCInteraciveAPITest >> text [
	
	^ text
]
