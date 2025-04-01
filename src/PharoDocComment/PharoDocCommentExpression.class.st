"
I am represent an expression within a String (typically the contents of a ASTCommentNode resp. the comment text of a method. I am used as expression or the expected expression value by a PharoDocCommentNode.
"
Class {
	#name : 'PharoDocCommentExpression',
	#superclass : 'Object',
	#instVars : [
		'source',
		'node',
		'cachedExpressionNode'
	],
	#category : 'PharoDocComment-Base',
	#package : 'PharoDocComment',
	#tag : 'Base'
}

{ #category : 'operation' }
PharoDocCommentExpression >> evaluate [
	"Note: it seems there is no easy way here to reuse in Opal the parsed AST and just compile&evaluate it."

	self isWellFormed ifFalse: [
		self error: self methodClass name , '>>#' , self methodSelector , ': Doc comment is not well formed. Content: ' , node sourceNode contents ].
	"we do not check for isFaulty here, as in the case of syntax errors, we want the evaluate to give us better error messages"

	^ self methodClass compiler
		  source: self source;
		  isScripting: true;
		  receiver: self methodClass;
		  evaluate
]

{ #category : 'accessing' }
PharoDocCommentExpression >> expressionNode [

	cachedExpressionNode ifNil: [
		cachedExpressionNode := self expressionNodeReal
	].
	^ cachedExpressionNode
]

{ #category : 'accessing' }
PharoDocCommentExpression >> expressionNodeReal [
	"Force the parsing of the expression node"

	^ self methodClass compiler
		  source: self source;
		  isScripting: true;
		  receiver: self methodClass;
		  parse
]

{ #category : 'testing' }
PharoDocCommentExpression >> isFaulty [

	"Check is the ast is OK and follows the 2 specific doccomments rules"

	^ self isNotWellFormed or: [ self expressionNode isFaulty ]
]

{ #category : 'testing' }
PharoDocCommentExpression >> isNotWellFormed [

	"Check is the ast follows the 2 specific doccomments rules"

	| cpt |

	"There should be exactly only one triple > message send"
	cpt := 0.
	self expressionNode nodesDo: [ :n |
		(n isMessage and: [ n selector == #>>> ]) ifTrue: [ cpt := cpt + 1 ]].
	cpt ~= 1 ifTrue: [ ^ true ].

	"`self expressionNode` is a ASTDoItMethodNode. The last statement should be a return of a binop of >>>"
	self expressionNode body statements last value selector ~= #>>> ifTrue: [ ^ true ].

	^false
]

{ #category : 'testing' }
PharoDocCommentExpression >> isWellFormed [

	^self isNotWellFormed not
]

{ #category : 'operation' }
PharoDocCommentExpression >> methodClass [
	^ node sourceNode methodNode methodClass
]

{ #category : 'operation' }
PharoDocCommentExpression >> methodSelector [

	^ node sourceNode methodNode selector
]

{ #category : 'accessing' }
PharoDocCommentExpression >> node [
	^ node
]

{ #category : 'accessing' }
PharoDocCommentExpression >> node: anObject [
	node := anObject
]

{ #category : 'printing' }
PharoDocCommentExpression >> printOn: aStream [
	super printOn: aStream.
	aStream nextPutAll: '('.
	aStream nextPutAll: self source.
	aStream nextPutAll: ')'
]

{ #category : 'accessing' }
PharoDocCommentExpression >> source [
	^ source
]

{ #category : 'accessing' }
PharoDocCommentExpression >> source: anObject [
	source := anObject
]
