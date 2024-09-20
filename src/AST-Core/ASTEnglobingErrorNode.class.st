"
I am a node representing a source code segment that parsed but never used in a node because of an unexpected error at the end. I am mainly used for source-code coloring and icon styling where all the code parsed should be colored normaly but underlined as part of the error.
This node also propose a reparation research.

Parsing faulty code without raising a syntax error is done by 
RBParser parseFaultyExpression:
or
RBParser parseFaultyMethod: 

Accessing to the parsed nodes contained inside the node is the method 'content'.

`valueAfter` is used to store the optional raw content (source string) that appears after the content.


"
Class {
	#name : 'ASTEnglobingErrorNode',
	#superclass : 'ASTParseErrorNode',
	#instVars : [
		'valueAfter',
		'contents'
	],
	#category : 'AST-Core-Nodes - ErrorNodes',
	#package : 'AST-Core',
	#tag : 'Nodes - ErrorNodes'
}

{ #category : 'construction' }
ASTEnglobingErrorNode class >> error: aToken withNodes: aCollection [

	"Realise a selection between the different englobing error node classes possible according to the
	 received token. If the value of the token is not recognised, we create an undetermined one."
	('()' includes: aToken value)
		ifTrue: [ ^ASTParenthesesErrorNode error: aToken withNodes: aCollection ].
	('[]' includes: aToken value)
		ifTrue: [ ^ASTBlockErrorNode error: aToken withNodes: aCollection ].
	('#(' = aToken value asString)
		ifTrue: [ ^ASTLiteralArrayErrorNode error: aToken withNodes: aCollection ].
	('#[' = aToken value asString)
		ifTrue: [ ^ASTLiteralByteArrayErrorNode error: aToken withNodes: aCollection ].
	('{}' includes: aToken value)
		ifTrue: [ ^ASTArrayErrorNode error: aToken withNodes: aCollection ].
	('|' = aToken value asString)
		ifTrue: [ ^ASTTemporariesErrorNode error: aToken withNodes: aCollection ].
	('<' = aToken value asString)
		ifTrue: [ ^ASTPragmaErrorNode error: aToken withNodes: aCollection ].
	^self new
		contents: aCollection;
		start: aCollection first start;
		stop: aToken stop;
		errorMessage: 'complementary of''',aToken value,''' expected'
]

{ #category : 'instance creation' }
ASTEnglobingErrorNode class >> from: aParseErrorNode contents: anArrayOfNodes [
	"Transform a simple error node into a englobing one while preserving most information"

	| start stop |
	start := aParseErrorNode start.
	stop := aParseErrorNode stop.
	anArrayOfNodes ifNotEmpty: [
		start := start min: (anArrayOfNodes min: #start).
		stop := stop max: (anArrayOfNodes max: #stop) ].

	^ self new
		start: start;
		stop: stop;
		errorPosition: aParseErrorNode errorPosition;
		errorMessage: aParseErrorNode errorMessage;
		valueAfter: aParseErrorNode value;
		contents: anArrayOfNodes
]

{ #category : 'visiting' }
ASTEnglobingErrorNode >> acceptVisitor: aVisitor [

	^ aVisitor visitEnglobingErrorNode: self
]

{ #category : 'accessing' }
ASTEnglobingErrorNode >> children [
	^ contents
]

{ #category : 'accessing' }
ASTEnglobingErrorNode >> contents [
	^contents
]

{ #category : 'accessing' }
ASTEnglobingErrorNode >> contents: aCollection [
	contents := aCollection.
	aCollection do: [:each | each parent: self ]
]

{ #category : 'initialization' }
ASTEnglobingErrorNode >> initialize [

	super initialize.
	contents := OrderedCollection new.
	value := ''.
	valueAfter := ''
]

{ #category : 'testing' }
ASTEnglobingErrorNode >> isEnglobingError [
	^true
]

{ #category : 'replacing' }
ASTEnglobingErrorNode >> replaceNode: aNode withNode: anotherNode [

	self contents: (contents collect: [ :each |
			 each == aNode
				 ifTrue: [ anotherNode ]
				 ifFalse: [ each ] ])
]

{ #category : 'accessing' }
ASTEnglobingErrorNode >> stop: anInterger [
	stop := anInterger
]

{ #category : 'accessing' }
ASTEnglobingErrorNode >> valueAfter [

	^ valueAfter
]

{ #category : 'accessing' }
ASTEnglobingErrorNode >> valueAfter: anObject [

	valueAfter := anObject
]
