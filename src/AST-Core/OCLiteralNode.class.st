"
ASTLiteralNode is an AST node that represents literals.

Instance Variables
	start: <Integer | nil> source position for the literal's beginning
	stop: <Integer | nil> source position for the literal's end
"
Class {
	#name : 'OCLiteralNode',
	#superclass : 'OCValueNode',
	#instVars : [
		'start',
		'stop'
	],
	#category : 'AST-Core-Nodes',
	#package : 'AST-Core',
	#tag : 'Nodes'
}

{ #category : 'testing' }
OCLiteralNode class >> isAbstract [

	^ self == OCLiteralNode
]

{ #category : 'instance creation' }
OCLiteralNode class >> value: aValue [
	"we check here for Array and ByteArray explicitly, as subclasses should use ASTLiteralNode.
	using #isByteArray would be inherited by subclasses"
	^((aValue class == Array or: [aValue class == ByteArray])
		ifTrue: [OCLiteralArrayNode]
		ifFalse: [OCLiteralValueNode]) value: aValue
]

{ #category : 'comparing' }
OCLiteralNode >> = anObject [
	self == anObject ifTrue: [^true].
	^self class = anObject class
]

{ #category : 'visiting' }
OCLiteralNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitLiteralNode: self
]

{ #category : 'comparing' }
OCLiteralNode >> hash [
	^self value hash
]

{ #category : 'initialization' }
OCLiteralNode >> initialize [

	super initialize.

	start := 0.
	stop := 0
]

{ #category : 'testing' }
OCLiteralNode >> isImmediateNode [
	^true
]

{ #category : 'testing' }
OCLiteralNode >> isLiteralNode [
	^true
]

{ #category : 'testing' }
OCLiteralNode >> needsParenthesis [
	^false
]

{ #category : 'accessing' }
OCLiteralNode >> precedence [
	^0
]

{ #category : 'private - replacing' }
OCLiteralNode >> replaceSourceFrom: aNode [
	self addReplacement: (OCStringReplacement
				replaceFrom: aNode start
				to: aNode stop
				with: self formattedCode)
]

{ #category : 'private - replacing' }
OCLiteralNode >> replaceSourceWith: aNode [
	self addReplacement: (OCStringReplacement
				replaceFrom: self start
				to: self stop
				with: aNode formattedCode)
]

{ #category : 'accessing' }
OCLiteralNode >> start: anInteger [
	start := anInteger
]

{ #category : 'accessing' }
OCLiteralNode >> startWithoutParentheses [
	^start
]

{ #category : 'accessing' }
OCLiteralNode >> stop: anInteger [
	stop := anInteger
]

{ #category : 'accessing' }
OCLiteralNode >> stopWithoutParentheses [
	^stop
]

{ #category : 'accessing' }
OCLiteralNode >> value [
	^self subclassResponsibility
]
