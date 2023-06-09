"
I build a selfcall tree from a class and a selector.

The self call tree is as big as possible. That is to say, if calls are made in conditional blocks, they are taken in account independently from the value of the conditional.

See:
	self example
"
Class {
	#name : #RTFSelfCallInterpreter,
	#superclass : #Object,
	#instVars : [
		'selfClass',
		'selfSentMethods',
		'considerClassesThat',
		'callStack',
		'rootSelfCall'
	],
	#category : #'RottenTestsFinder-SelfInterpreter'
}

{ #category : #example }
RTFSelfCallInterpreter class >> example [
	^ RTFSelfCallInterpreter new
		considerClassesThat: [ :class | class inheritsFrom: Collection ];
		send: #select: fromClass: OrderedCollection;
		rootSelfCall
]

{ #category : #accessing }
RTFSelfCallInterpreter >> considerClassesThat [
	^ considerClassesThat
]

{ #category : #accessing }
RTFSelfCallInterpreter >> considerClassesThat: anObject [
	considerClassesThat := anObject
]

{ #category : #initialization }
RTFSelfCallInterpreter >> initialize [
	super initialize.
	selfSentMethods := OrderedCollection new.
	callStack := Stack new
]

{ #category : #accessing }
RTFSelfCallInterpreter >> rootSelfCall [
	^ rootSelfCall
]

{ #category : #accessing }
RTFSelfCallInterpreter >> rootSelfCall: anObject [
	rootSelfCall := anObject
]

{ #category : #accessing }
RTFSelfCallInterpreter >> selfSentMethods [
	^ selfSentMethods
]

{ #category : #api }
RTFSelfCallInterpreter >> send: aSelector fromClass: aClass [

	| method |
	selfClass := aClass.
	method := selfClass lookupSelector: aSelector.

	callStack push: (RTFSelfCallRootNode compiledMethod: method).
	method ast acceptVisitor: self.
	rootSelfCall := callStack pop
]

{ #category : #visiting }
RTFSelfCallInterpreter >> visitArgumentVariableNode: aRBArgumentNode [

	self visitVariableNode: aRBArgumentNode
]

{ #category : #visiting }
RTFSelfCallInterpreter >> visitArrayNode: aRBArrayNode [
	aRBArrayNode statements do: [ :statement | statement acceptVisitor: self ]
]

{ #category : #visiting }
RTFSelfCallInterpreter >> visitAssignmentNode: aRBAssignmentNode [

	aRBAssignmentNode value acceptVisitor: self
]

{ #category : #visiting }
RTFSelfCallInterpreter >> visitBlockNode: aRBBlockNode [

	aRBBlockNode statements do: [ :each |
		each acceptVisitor: self ]
]

{ #category : #visiting }
RTFSelfCallInterpreter >> visitCascadeNode: aRBCascadeNode [
	aRBCascadeNode messages do: [ :message |
		message acceptVisitor: self ]
]

{ #category : #visiting }
RTFSelfCallInterpreter >> visitClassVariableNode: aRBVariableNode [
	self visitVariableNode: aRBVariableNode.
	^ self
]

{ #category : #visiting }
RTFSelfCallInterpreter >> visitGlobalVariableNode: aRBGlobalNode [

	self visitVariableNode: aRBGlobalNode
]

{ #category : #visiting }
RTFSelfCallInterpreter >> visitInstanceVariableNode: aRBInstanceVariableNode [
	self visitVariableNode: aRBInstanceVariableNode.
	^ self
]

{ #category : #visiting }
RTFSelfCallInterpreter >> visitLiteralArrayNode: aRBLiteralArrayNode [
	"Does nothing."
	^ self
]

{ #category : #visiting }
RTFSelfCallInterpreter >> visitLiteralNode: aRBLiteralValueNode [

	"Do nothing with literals"
	^ self
]

{ #category : #visiting }
RTFSelfCallInterpreter >> visitLiteralValueNode: aRBLiteralValueNode [
	"Do nothing with literals"
	^ self
]

{ #category : #visiting }
RTFSelfCallInterpreter >> visitMessageNode: aRBMessageNode [
	| selfSentMethod callNode |
	aRBMessageNode receiver acceptVisitor: self.
	aRBMessageNode arguments do: [ :each | each acceptVisitor: self ].

	(aRBMessageNode receiver isSelfVariable or: [ aRBMessageNode receiver isSuperVariable ])
		ifFalse: [ ^ self ].

	selfSentMethod := (aRBMessageNode receiver isSuperVariable
		ifTrue: [ selfClass superclass ]
		ifFalse: [ selfClass ]) lookupSelector: aRBMessageNode selector.

	selfSentMethod class = RTFMethodTracer
		ifTrue: [ selfSentMethod := selfSentMethod method ].

	selfSentMethod
		ifNil: [
			Warning signal: ('#{1} can not be found in {2} nor in any super class.' format: { aRBMessageNode selector . selfClass name }).
			^ self ].

	callNode := RTFSelfCallNode astNode: aRBMessageNode compiledMethod: selfSentMethod.
	callStack top addCall: callNode.

	(considerClassesThat value: selfSentMethod methodClass)
		ifFalse: [ ^ self ].
	selfSentMethod isPrimitive
		ifTrue: [ ^ self ].
	(selfSentMethods includes: selfSentMethod)
		ifTrue: [ ^ self ].

	selfSentMethods add: selfSentMethod.


	callStack push: callNode.
	selfSentMethod ast acceptVisitor: self.
	callStack pop
]

{ #category : #visiting }
RTFSelfCallInterpreter >> visitMethodNode: aRBMethodNode [
	aRBMethodNode statements do: [ :each |
		each acceptVisitor: self ]
]

{ #category : #visiting }
RTFSelfCallInterpreter >> visitReturnNode: aRBReturnNode [

	aRBReturnNode value acceptVisitor: self
]

{ #category : #visiting }
RTFSelfCallInterpreter >> visitSelfNode: aRBSelfNode [

	self visitVariableNode: aRBSelfNode
]

{ #category : #visiting }
RTFSelfCallInterpreter >> visitSuperNode: aRBSuperNode [
	self visitVariableNode: aRBSuperNode
]

{ #category : #visiting }
RTFSelfCallInterpreter >> visitTemporaryVariableNode: aRBTemporaryNode [

	self visitVariableNode: aRBTemporaryNode
]

{ #category : #visiting }
RTFSelfCallInterpreter >> visitThisContextNode: aRBThisContextNode [

	self visitVariableNode: aRBThisContextNode
]

{ #category : #visiting }
RTFSelfCallInterpreter >> visitVariableNode: aRBArgumentNode [

	"Do nothing with variables"
	^ self
]
