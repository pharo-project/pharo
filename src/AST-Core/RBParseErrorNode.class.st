"
I am a node representing a source code segment that could not be parsed. I am mainly used for source-code coloring where we should parse as far as possible and mark the rest as a failure.

Parsing faulty code without raising a syntax error is done by 
RBParser parseFaultyExpression:
or
RBParser parseFaultyMethod: 

The return value is either valid nodes representing the AST, or nodes representing the valid portion and an RBParseErrorNode for the remaining invalid code.


"
Class {
	#name : #RBParseErrorNode,
	#superclass : #RBValueNode,
	#instVars : [
		'errorMessage',
		'value',
		'start'
	],
	#category : #'AST-Core-Nodes'
}

{ #category : #'instance creation' }
RBParseErrorNode class >> errorMessage: aString value: aValue at: aPosition [

	^ self new
		errorMessage: aString;
		value: aValue;
		start: aPosition;
		yourself
]

{ #category : #comparing }
RBParseErrorNode >> = anObject [ 
	self == anObject ifTrue: [^true].
	self class = anObject class ifFalse: [^false].
	^anObject value = value and: [ anObject errorMessage = errorMessage ]
]

{ #category : #visiting }
RBParseErrorNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitParseErrorNode: self
]

{ #category : #converting }
RBParseErrorNode >> adaptToSemanticNode [
	" I can't provide more semantics "
]

{ #category : #converting }
RBParseErrorNode >> asSyntaxErrorNotification [
	^SyntaxErrorNotification new
		setClass: self methodNode methodClass
		code: self methodNode source
		doitFlag: false
		errorMessage: errorMessage
		location: self start
]

{ #category : #accessing }
RBParseErrorNode >> binding: anOCTempVariable [ 
	"only for compatibility"
	
]

{ #category : #accessing }
RBParseErrorNode >> body: aSequenceNode [
	"I am not a valid MethodNode, but go one with parsing"
]

{ #category : #accessing }
RBParseErrorNode >> errorMessage [
	^ errorMessage
]

{ #category : #accessing }
RBParseErrorNode >> errorMessage: anObject [
	errorMessage := anObject
]

{ #category : #comparing }
RBParseErrorNode >> hash [
	^ (self value hash bitXor: self errorMessage hash)
]

{ #category : #testing }
RBParseErrorNode >> isFaulty [
	^true
]

{ #category : #testing }
RBParseErrorNode >> isParseError [
	^true
]

{ #category : #accessing }
RBParseErrorNode >> name [
	"be polymorphic with variable nodes"

	^ ''
]

{ #category : #testing }
RBParseErrorNode >> needsParenthesis [
	^ false
]

{ #category : #accessing }
RBParseErrorNode >> source: aString [
	"I am not a valid MethodNode, but go one with parsing"
]

{ #category : #accessing }
RBParseErrorNode >> start [
	^ start
]

{ #category : #accessing }
RBParseErrorNode >> start: aPosition [
	start := aPosition
]

{ #category : #accessing }
RBParseErrorNode >> startWithoutParentheses [
	^ start
]

{ #category : #accessing }
RBParseErrorNode >> stop [
	^ start + value size - 1.
]

{ #category : #accessing }
RBParseErrorNode >> stopWithoutParentheses [
	^ self stop
]

{ #category : #evaluating }
RBParseErrorNode >> value [
	^value.
]

{ #category : #accessing }
RBParseErrorNode >> value: aString [
	value := aString.
]
