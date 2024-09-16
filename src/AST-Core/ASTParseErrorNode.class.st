"
I am a node representing a source code segment that could not be parsed. I am mainly used for source-code coloring where we should parse as far as possible and mark the rest as a failure.

Parsing faulty code without raising a syntax error is done by 
RBParser parseFaultyExpression:
or
RBParser parseFaultyMethod: 

The return value is either valid nodes representing the AST, or nodes representing the valid portion and an RBParseErrorNode for the remaining invalid code.


"
Class {
	#name : 'ASTParseErrorNode',
	#superclass : 'ASTValueNode',
	#instVars : [
		'errorMessage',
		'value',
		'start',
		'stop',
		'errorPosition'
	],
	#category : 'AST-Core-Nodes - ErrorNodes',
	#package : 'AST-Core',
	#tag : 'Nodes - ErrorNodes'
}

{ #category : 'comparing' }
ASTParseErrorNode >> = anObject [
	self == anObject ifTrue: [^true].
	self class = anObject class ifFalse: [^false].
	^anObject value = value and: [ anObject errorMessage = errorMessage ]
]

{ #category : 'visiting' }
ASTParseErrorNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitParseErrorNode: self
]

{ #category : 'accessing' }
ASTParseErrorNode >> arguments [

	"A parse error node has no arguments"
	^ #()
]

{ #category : 'accessing' }
ASTParseErrorNode >> binding: anOCTempVariable [
	"only for compatibility"
]

{ #category : 'accessing' }
ASTParseErrorNode >> body: aSequenceNode [
	"I am not a valid MethodNode, but go one with parsing"
]

{ #category : 'accessing' }
ASTParseErrorNode >> errorMessage [
	^ errorMessage
]

{ #category : 'accessing' }
ASTParseErrorNode >> errorMessage: anObject [
	errorMessage := anObject
]

{ #category : 'accessing' }
ASTParseErrorNode >> errorPosition [

	^ errorPosition
]

{ #category : 'accessing' }
ASTParseErrorNode >> errorPosition: anObject [

	errorPosition := anObject
]

{ #category : 'testing' }
ASTParseErrorNode >> hasTemporaries [

	^ false
]

{ #category : 'comparing' }
ASTParseErrorNode >> hash [
	^ (self value hash bitXor: self errorMessage hash)
]

{ #category : 'initialization' }
ASTParseErrorNode >> initialize [

	super initialize.
	"Add its own notice"
	self addNotice: (RBSyntaxErrorNotice new)
]

{ #category : 'errors' }
ASTParseErrorNode >> isError [
	^true
]

{ #category : 'errors' }
ASTParseErrorNode >> isFaulty [
	^true
]

{ #category : 'testing' }
ASTParseErrorNode >> isParseError [
	^true
]

{ #category : 'testing' }
ASTParseErrorNode >> isUnary [

	^ false
]

{ #category : 'accessing' }
ASTParseErrorNode >> name [
	"be polymorphic with variable nodes"

	^ ''
]

{ #category : 'testing' }
ASTParseErrorNode >> needsParenthesis [
	^ false
]

{ #category : 'accessing' }
ASTParseErrorNode >> selector [
	"A parse error node has an empty selector"
	^ #''
]

{ #category : 'accessing' }
ASTParseErrorNode >> selectorParts [

	^ #()
]

{ #category : 'accessing' }
ASTParseErrorNode >> source: aString [
	"I am not a valid MethodNode, but go one with parsing"
]

{ #category : 'accessing' }
ASTParseErrorNode >> start: aPosition [
	"Beware, start is in fact `startWithoutParentheses` as in ASTValueNode, start includes parentheses"

	start := aPosition
]

{ #category : 'accessing' }
ASTParseErrorNode >> startWithoutParentheses [
	^ start
]

{ #category : 'accessing' }
ASTParseErrorNode >> stop: aStopPosition [
	"Beware, stop is in fact `stopWithoutParentheses` as in ASTValueNode, stop includes parentheses"

	stop := aStopPosition
]

{ #category : 'accessing' }
ASTParseErrorNode >> stopWithoutParentheses [
	^ stop
		ifNil: [start + value size - 1]
		ifNotNil: [ stop ]
]

{ #category : 'accessing' }
ASTParseErrorNode >> temporaries [

	^ #()
]

{ #category : 'evaluating' }
ASTParseErrorNode >> value [
	^value
]

{ #category : 'accessing' }
ASTParseErrorNode >> value: aString [
	value := aString
]
