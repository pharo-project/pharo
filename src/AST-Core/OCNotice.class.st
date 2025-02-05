"
I represent error and warning information on a AST node.
"
Class {
	#name : 'OCNotice',
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
OCNotice >> <= other [

	self errorLevel = other errorLevel ifFalse: [
		^ self errorLevel > other errorLevel ]. "Errors before warnings"
	^ self position <= other position
]

{ #category : 'accessing' }
OCNotice >> description [

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
OCNotice >> errorLevel [

	self isSyntaxError ifTrue: [ ^ 3 ].
	self isError ifTrue: [ ^ 2 ].
	self isWarning ifTrue: [ ^ 1 ].
	^ 0
]

{ #category : 'inspecting' }
OCNotice >> inspectionSource [

	<inspectorPresentationOrder: 30 title: 'Source'>
	^ node inspectionSource
]

{ #category : 'testing' }
OCNotice >> isError [

	^ false
]

{ #category : 'testing' }
OCNotice >> isSyntaxError [

	^ false
]

{ #category : 'testing' }
OCNotice >> isUndeclaredNotice [

	^ false
]

{ #category : 'testing' }
OCNotice >> isWarning [

	^ false
]

{ #category : 'accessing' }
OCNotice >> messageText [

	^ messageText
]

{ #category : 'signaling' }
OCNotice >> messageText: aString [

	messageText := aString
]

{ #category : 'accessing' }
OCNotice >> node [

	^ node
]

{ #category : 'accessing' }
OCNotice >> node: anObject [

	node := anObject
]

{ #category : 'error handling' }
OCNotice >> position [

	^ node start
]

{ #category : 'printing' }
OCNotice >> printOn: aStream [

	aStream
		nextPutAll: self class name;
		nextPutAll: '(';
		print: self position;
		nextPutAll: ':';
		nextPutAll: self messageText;
		nextPutAll: ')'
]

{ #category : 'signalling' }
OCNotice >> signalError [
	"If a debugger is opened here, it means that a code error (syntax error or semantic error)
	was uncaught while compiling some code.
	Inspect `self` to get a view of the problematic source code.

	Resuming the error (proceed) will continue the compilation anyway, but might produce an
	executable code that will signal runtime errors at runtime.
	However, it allows you to repair and recompile the code at your own time.

	If you close the debugger, then we will all forgot this ever happen."

	OCCodeError new
		notice: self;
		signal
]
