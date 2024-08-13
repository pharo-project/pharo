"
ASTLiteralNode is an AST node that represents literals.

Instance Variables
	start: <Integer | nil> source position for the literal's beginning
	stop: <Integer | nil> source position for the literal's end
"
Class {
	#name : 'ASTLiteralNode',
	#superclass : 'ASTValueNode',
	#instVars : [
		'start',
		'stop'
	],
	#category : 'AST-Core-Nodes',
	#package : 'AST-Core',
	#tag : 'Nodes'
}

{ #category : 'testing' }
ASTLiteralNode class >> isAbstract [

	^ self == ASTLiteralNode
]

{ #category : 'instance creation' }
ASTLiteralNode class >> value: aValue [
	"we check here for Array and ByteArray explicitly, as subclasses should use ASTLiteralNode.
	using #isByteArray would be inherited by subclasses"
	^((aValue class == Array or: [aValue class == ByteArray])
		ifTrue: [RBLiteralArrayNode]
		ifFalse: [RBLiteralValueNode]) value: aValue
]

{ #category : 'comparing' }
ASTLiteralNode >> = anObject [
	self == anObject ifTrue: [^true].
	^self class = anObject class
]

{ #category : 'visiting' }
ASTLiteralNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitLiteralNode: self
]

{ #category : 'comparing' }
ASTLiteralNode >> hash [
	^self value hash
]

{ #category : 'initialization' }
ASTLiteralNode >> initialize [

	super initialize.

	start := 0.
	stop := 0
]

{ #category : 'testing' }
ASTLiteralNode >> isImmediateNode [
	^true
]

{ #category : 'testing' }
ASTLiteralNode >> isLiteralNode [
	^true
]

{ #category : 'testing' }
ASTLiteralNode >> needsParenthesis [
	^false
]

{ #category : 'accessing' }
ASTLiteralNode >> precedence [
	^0
]

{ #category : 'private - replacing' }
ASTLiteralNode >> replaceSourceFrom: aNode [
	self addReplacement: (RBStringReplacement
				replaceFrom: aNode start
				to: aNode stop
				with: self formattedCode)
]

{ #category : 'private - replacing' }
ASTLiteralNode >> replaceSourceWith: aNode [
	self addReplacement: (RBStringReplacement
				replaceFrom: self start
				to: self stop
				with: aNode formattedCode)
]

{ #category : 'accessing' }
ASTLiteralNode >> start: anInteger [
	start := anInteger
]

{ #category : 'accessing' }
ASTLiteralNode >> startWithoutParentheses [
	^start
]

{ #category : 'accessing' }
ASTLiteralNode >> stop: anInteger [
	stop := anInteger
]

{ #category : 'accessing' }
ASTLiteralNode >> stopWithoutParentheses [
	^stop
]

{ #category : 'accessing' }
ASTLiteralNode >> value [
	^self subclassResponsibility
]
