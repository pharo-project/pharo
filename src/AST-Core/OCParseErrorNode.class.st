"
I am a node representing a source code segment that could not be parsed. I am mainly used for source-code coloring where we should parse as far as possible and mark the rest as a failure.

Parsing faulty code without raising a syntax error is done by 
RBParser parseFaultyExpression:
or
RBParser parseFaultyMethod: 

The return value is either valid nodes representing the AST, or nodes representing the valid portion and an ASTParseErrorNode for the remaining invalid code.


"
Class {
	#name : 'OCParseErrorNode',
	#superclass : 'OCValueNode',
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
OCParseErrorNode >> = anObject [
	self == anObject ifTrue: [^true].
	self class = anObject class ifFalse: [^false].
	^anObject value = value and: [ anObject errorMessage = errorMessage ]
]

{ #category : 'visiting' }
OCParseErrorNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitParseErrorNode: self
]

{ #category : 'accessing' }
OCParseErrorNode >> arguments [

	"A parse error node has no arguments"
	^ #()
]

{ #category : 'accessing' }
OCParseErrorNode >> binding: anOCTempVariable [
	"only for compatibility"
]

{ #category : 'accessing' }
OCParseErrorNode >> body: aSequenceNode [
	"I am not a valid MethodNode, but go one with parsing"
]

{ #category : 'accessing' }
OCParseErrorNode >> errorMessage [
	^ errorMessage
]

{ #category : 'accessing' }
OCParseErrorNode >> errorMessage: anObject [
	errorMessage := anObject
]

{ #category : 'accessing' }
OCParseErrorNode >> errorPosition [

	^ errorPosition
]

{ #category : 'accessing' }
OCParseErrorNode >> errorPosition: anObject [

	errorPosition := anObject
]

{ #category : 'testing' }
OCParseErrorNode >> hasTemporaries [

	^ false
]

{ #category : 'comparing' }
OCParseErrorNode >> hash [
	^ (self value hash bitXor: self errorMessage hash)
]

{ #category : 'initialization' }
OCParseErrorNode >> initialize [

	super initialize.
	"Add its own notice"
	self addNotice: (OCSyntaxErrorNotice new)
]

{ #category : 'errors' }
OCParseErrorNode >> isError [
	^true
]

{ #category : 'errors' }
OCParseErrorNode >> isFaulty [
	^true
]

{ #category : 'testing' }
OCParseErrorNode >> isParseError [
	^true
]

{ #category : 'testing' }
OCParseErrorNode >> isUnary [

	^ false
]

{ #category : 'accessing' }
OCParseErrorNode >> name [
	"be polymorphic with variable nodes"

	^ ''
]

{ #category : 'testing' }
OCParseErrorNode >> needsParenthesis [
	^ false
]

{ #category : 'accessing' }
OCParseErrorNode >> selector [
	"A parse error node has an empty selector"
	^ #''
]

{ #category : 'accessing' }
OCParseErrorNode >> selectorParts [

	^ #()
]

{ #category : 'accessing' }
OCParseErrorNode >> source: aString [
	"I am not a valid MethodNode, but go one with parsing"
]

{ #category : 'accessing' }
OCParseErrorNode >> start: aPosition [
	"Beware, start is in fact `startWithoutParentheses` as in ASTValueNode, start includes parentheses"

	start := aPosition
]

{ #category : 'accessing' }
OCParseErrorNode >> startWithoutParentheses [
	^ start
]

{ #category : 'accessing' }
OCParseErrorNode >> stop: aStopPosition [
	"Beware, stop is in fact `stopWithoutParentheses` as in ASTValueNode, stop includes parentheses"

	stop := aStopPosition
]

{ #category : 'accessing' }
OCParseErrorNode >> stopWithoutParentheses [
	^ stop
		ifNil: [start + value size - 1]
		ifNotNil: [ stop ]
]

{ #category : 'accessing' }
OCParseErrorNode >> temporaries [

	^ #()
]

{ #category : 'evaluating' }
OCParseErrorNode >> value [
	^value
]

{ #category : 'accessing' }
OCParseErrorNode >> value: aString [
	value := aString
]
