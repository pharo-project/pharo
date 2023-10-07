"
I represent error and warning information on a AST node.
"
Class {
	#name : 'RBNotice',
	#superclass : 'Object',
	#instVars : [
		'messageText',
		'node'
	],
	#category : 'AST-Core-Notice',
	#package : 'AST-Core',
	#tag : 'Notice'
}

{ #category : 'comparing' }
RBNotice >> <= other [

	self errorLevel = other errorLevel ifFalse: [
		^ self errorLevel > other errorLevel ]. "Errors before warnings"
	^ self position <= other position
]

{ #category : 'accessing' }
RBNotice >> description [

	^ String streamContents: [ :stream |
		  stream
			  nextPutAll: self class name;
			  nextPutAll: ' '.
		  node methodNode
			  ifNotNil: [ :methodNode |
				  stream
					  nextPutAll: (methodNode methodClass
							   ifNotNil: [ :class | class name ]
							   ifNil: [ '???' ]);
					  nextPutAll: '>>#';
					  nextPutAll: node methodNode selector ]
			  ifNil: [ '???>>#???' ].
		  stream
			  nextPutAll: ' ';
			  print: self position;
			  nextPutAll: ':';
			  nextPutAll: self messageText.
		  node methodNode ifNotNil: [
			  stream
				  nextPutAll: '->';
				  nextPutAll:
					  (node sourceCode asString withBlanksCondensed
						   truncateWithElipsisTo: 60) ] ]
]

{ #category : 'error handling' }
RBNotice >> errorLevel [

	self isSyntaxError ifTrue: [ ^ 3 ].
	self isError ifTrue: [ ^ 2 ].
	self isWarning ifTrue: [ ^ 1 ].
	^ 0
]

{ #category : 'inspecting' }
RBNotice >> inspectionSource [

	<inspectorPresentationOrder: 30 title: 'Source'>
	^ node inspectionSource
]

{ #category : 'testing' }
RBNotice >> isError [

	^ false
]

{ #category : 'testing' }
RBNotice >> isSyntaxError [

	^ false
]

{ #category : 'testing' }
RBNotice >> isUndeclaredNotice [

	^ false
]

{ #category : 'testing' }
RBNotice >> isWarning [

	^ false
]

{ #category : 'accessing' }
RBNotice >> messageText [

	^ messageText
]

{ #category : 'signaling' }
RBNotice >> messageText: aString [

	messageText := aString
]

{ #category : 'accessing' }
RBNotice >> node [

	^ node
]

{ #category : 'accessing' }
RBNotice >> node: anObject [

	node := anObject
]

{ #category : 'error handling' }
RBNotice >> position [

	^ node start
]

{ #category : 'printing' }
RBNotice >> printOn: aStream [

	aStream
		nextPutAll: self class name;
		nextPutAll: '(';
		print: self position;
		nextPutAll: ':';
		nextPutAll: self messageText;
		nextPutAll: ')'
]

{ #category : 'signalling' }
RBNotice >> signalError [
	"If a debugger is opened here, it means that a code error (syntax error or semantic error)
	was uncaught while compiling some code.
	Inspect `self` to get a view of the problematic source code.

	Resuming the error (proceed) will continue the compilation anyway, but might produce an
	executable code that will signal runtime errors at runtime.
	However, it allows you to repair and recompile the code at your own time.

	If you close the debugger, then we will all forgot this ever happen."

	CodeError new
		notice: self;
		signal
]
