"
RBLiteralNode is an AST node that represents literals.

Instance Variables
	start: <Integer | nil> source position for the literal's beginning
	stop: <Integer | nil> source position for the literal's end
"
Class {
	#name : #RBLiteralNode,
	#superclass : #RBValueNode,
	#instVars : [
		'start',
		'stop'
	],
	#category : #'AST-Core-Nodes'
}

{ #category : #testing }
RBLiteralNode class >> isAbstract [

	^ self == RBLiteralNode
]

{ #category : #'instance creation' }
RBLiteralNode class >> value: aValue [
	"we check here for Array and ByteArray explicitly, as subclasses should use RBLiteralNode.
	using #isByteArray would be inherited by subclasses"
	^((aValue class == Array or: [aValue class == ByteArray])
		ifTrue: [RBLiteralArrayNode]
		ifFalse: [RBLiteralValueNode]) value: aValue
]

{ #category : #comparing }
RBLiteralNode >> = anObject [
	self == anObject ifTrue: [^true].
	^self class = anObject class
]

{ #category : #visiting }
RBLiteralNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitLiteralNode: self
]

{ #category : #comparing }
RBLiteralNode >> hash [
	^self value hash
]

{ #category : #initialization }
RBLiteralNode >> initialize [

	super initialize.

	start := 0.
	stop := 0
]

{ #category : #testing }
RBLiteralNode >> isImmediateNode [
	^true
]

{ #category : #testing }
RBLiteralNode >> isLiteralNode [
	^true
]

{ #category : #testing }
RBLiteralNode >> needsParenthesis [
	^false
]

{ #category : #accessing }
RBLiteralNode >> precedence [
	^0
]

{ #category : #'private - replacing' }
RBLiteralNode >> replaceSourceFrom: aNode [
	self addReplacement: (RBStringReplacement
				replaceFrom: aNode start
				to: aNode stop
				with: self formattedCode)
]

{ #category : #'private - replacing' }
RBLiteralNode >> replaceSourceWith: aNode [
	self addReplacement: (RBStringReplacement
				replaceFrom: self start
				to: self stop
				with: aNode formattedCode)
]

{ #category : #accessing }
RBLiteralNode >> start: anInteger [
	start := anInteger
]

{ #category : #accessing }
RBLiteralNode >> startWithoutParentheses [
	^start
]

{ #category : #accessing }
RBLiteralNode >> stop: anInteger [
	stop := anInteger
]

{ #category : #accessing }
RBLiteralNode >> stopWithoutParentheses [
	^stop
]

{ #category : #accessing }
RBLiteralNode >> value [
	^self subclassResponsibility
]
