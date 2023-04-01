"
I represent error and warning information on a AST node.
"
Class {
	#name : #RBNotice,
	#superclass : #Object,
	#instVars : [
		'messageText',
		'node'
	],
	#category : #'AST-Core-Notice'
}

{ #category : #accessing }
RBNotice >> description [

	^ String streamContents: [ :stream |
		  stream
			  nextPutAll: self class name;
			  nextPutAll: ' ';
			  nextPutAll: (node methodNode methodClass
					   ifNotNil: [ :class | class name ]
					   ifNil: [ '???' ]);
			  nextPutAll: '>>#';
			  nextPutAll: node methodNode selector;
			  nextPutAll: ' ';
			  print: self position;
			  nextPutAll: ':';
			  nextPutAll: self messageText;
			  nextPutAll: '->';
			  nextPutAll: (node sourceCode asString withBlanksCondensed truncateWithElipsisTo: 60) ]
]

{ #category : #testing }
RBNotice >> isError [

	^ false
]

{ #category : #testing }
RBNotice >> isWarning [

	^ false
]

{ #category : #accessing }
RBNotice >> messageText [

	^ messageText
]

{ #category : #signaling }
RBNotice >> messageText: aString [

	messageText := aString
]

{ #category : #accessing }
RBNotice >> node [

	^ node
]

{ #category : #accessing }
RBNotice >> node: anObject [

	node := anObject
]

{ #category : #'error handling' }
RBNotice >> position [

	^ node start
]

{ #category : #printing }
RBNotice >> printOn: aStream [

	aStream
		nextPutAll: self class name;
		nextPutAll: '(';
		print: self position;
		nextPutAll: ':';
		nextPutAll: self messageText;
		nextPutAll: ')'
]

{ #category : #signalling }
RBNotice >> signalError [

	CodeError new
		notice: self;
		signal
]
