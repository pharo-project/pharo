"
I am a node representing a source code segment that parsed but never used in a node because of an unexpected error at the end. I am mainly used for source-code coloring and icon styling where all the code parsed should be colored normaly but underlined as part of the error.
This node also propose a reparation research.

Parsing faulty code without raising a syntax error is done by 
ASTParser parseFaultyExpression:
or
ASTParser parseFaultyMethod: 

Accessing to the parsed nodes contained inside the node is the method 'content'.


"
Class {
	#name : #ASTEnglobingErrorNode,
	#superclass : #ASTParseErrorNode,
	#instVars : [
		'content'
	],
	#category : #'AST-Core-Nodes - ErrorNodes'
}

{ #category : #'instance creation' }
ASTEnglobingErrorNode class >> content: aCollection start: aStart stop: aStop errorMessage: message [
	^self new 
		content: aCollection; 
		start: aStart; 
		stop: aStop; 
		errorMessage: message
]

{ #category : #construction }
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
	^self new content: aCollection; start: aCollection first start; stop: aToken stop; errorMessage: 'complementary of''',aToken value,''' expected'
]

{ #category : #visiting }
ASTEnglobingErrorNode >> acceptVisitor: aVisitor [

	^ aVisitor visitEnglobingErrorNode: self
]

{ #category : #accessing }
ASTEnglobingErrorNode >> children [
	^ content
]

{ #category : #accessing }
ASTEnglobingErrorNode >> content [
	^content
]

{ #category : #accessing }
ASTEnglobingErrorNode >> content: aCollection [
	content := aCollection.
	aCollection do: [:each | each parent: self ]
]

{ #category : #initialization }
ASTEnglobingErrorNode >> initialize [ 
	super initialize.
	content := OrderedCollection new.
	value := ''.
]

{ #category : #testing }
ASTEnglobingErrorNode >> isEnglobingError [
	^true
]

{ #category : #accessing }
ASTEnglobingErrorNode >> stop: anInterger [
	stop := anInterger.
]
