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
	#name : 'OCEnglobingErrorNode',
	#superclass : 'OCParseErrorNode',
	#instVars : [
		'valueAfter',
		'contents'
	],
	#category : 'AST-Core-Nodes - ErrorNodes',
	#package : 'AST-Core',
	#tag : 'Nodes - ErrorNodes'
}

{ #category : 'construction' }
OCEnglobingErrorNode class >> error: aToken withNodes: aCollection [

	"Realise a selection between the different englobing error node classes possible according to the
	 received token. If the value of the token is not recognised, we create an undetermined one."
	('()' includes: aToken value)
		ifTrue: [ ^OCParenthesesErrorNode error: aToken withNodes: aCollection ].
	('[]' includes: aToken value)
		ifTrue: [ ^OCBlockErrorNode error: aToken withNodes: aCollection ].
	('#(' = aToken value asString)
		ifTrue: [ ^OCLiteralArrayErrorNode error: aToken withNodes: aCollection ].
	('#[' = aToken value asString)
		ifTrue: [ ^OCLiteralByteArrayErrorNode error: aToken withNodes: aCollection ].
	('{}' includes: aToken value)
		ifTrue: [ ^OCArrayErrorNode error: aToken withNodes: aCollection ].
	('|' = aToken value asString)
		ifTrue: [ ^OCTemporariesErrorNode error: aToken withNodes: aCollection ].
	('<' = aToken value asString)
		ifTrue: [ ^OCPragmaErrorNode error: aToken withNodes: aCollection ].
	^self new
		contents: aCollection;
		start: aCollection first start;
		stop: aToken stop;
		errorMessage: 'complementary of''',aToken value,''' expected'
]

{ #category : 'instance creation' }
OCEnglobingErrorNode class >> from: aParseErrorNode contents: anArrayOfNodes [
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
OCEnglobingErrorNode >> acceptVisitor: aVisitor [

	^ aVisitor visitEnglobingErrorNode: self
]

{ #category : 'accessing' }
OCEnglobingErrorNode >> children [
	^ contents
]

{ #category : 'accessing' }
OCEnglobingErrorNode >> contents [
	^contents
]

{ #category : 'accessing' }
OCEnglobingErrorNode >> contents: aCollection [
	contents := aCollection.
	aCollection do: [:each | each parent: self ]
]

{ #category : 'initialization' }
OCEnglobingErrorNode >> initialize [

	super initialize.
	contents := OrderedCollection new.
	value := ''.
	valueAfter := ''
]

{ #category : 'testing' }
OCEnglobingErrorNode >> isEnglobingError [
	^true
]

{ #category : 'replacing' }
OCEnglobingErrorNode >> replaceNode: aNode withNode: anotherNode [

	self contents: (contents collect: [ :each |
			 each == aNode
				 ifTrue: [ anotherNode ]
				 ifFalse: [ each ] ])
]

{ #category : 'accessing' }
OCEnglobingErrorNode >> stop: anInterger [
	stop := anInterger
]

{ #category : 'accessing' }
OCEnglobingErrorNode >> valueAfter [

	^ valueAfter
]

{ #category : 'accessing' }
OCEnglobingErrorNode >> valueAfter: anObject [

	valueAfter := anObject
]
