"
I build a selfcall tree from a class and a selector.

The self call tree is as big as possible. That is to say, if calls are made in conditional blocks, they are taken in account independently from the value of the conditional.

See:
	self example
"
Class {
	#name : 'RTFSelfCallInterpreter',
	#superclass : 'OCProgramNodeVisitor',
	#instVars : [
		'selfClass',
		'selfSentMethods',
		'considerClassesThat',
		'callStack',
		'rootSelfCall'
	],
	#category : 'RottenTestsFinder-SelfInterpreter',
	#package : 'RottenTestsFinder',
	#tag : 'SelfInterpreter'
}

{ #category : 'example' }
RTFSelfCallInterpreter class >> example [
	^ RTFSelfCallInterpreter new
		considerClassesThat: [ :class | class inheritsFrom: Collection ];
		send: #select: fromClass: OrderedCollection;
		rootSelfCall
]

{ #category : 'accessing' }
RTFSelfCallInterpreter >> considerClassesThat [
	^ considerClassesThat
]

{ #category : 'accessing' }
RTFSelfCallInterpreter >> considerClassesThat: anObject [
	considerClassesThat := anObject
]

{ #category : 'initialization' }
RTFSelfCallInterpreter >> initialize [
	super initialize.
	selfSentMethods := OrderedCollection new.
	callStack := Stack new
]

{ #category : 'accessing' }
RTFSelfCallInterpreter >> rootSelfCall [
	^ rootSelfCall
]

{ #category : 'accessing' }
RTFSelfCallInterpreter >> rootSelfCall: anObject [
	rootSelfCall := anObject
]

{ #category : 'accessing' }
RTFSelfCallInterpreter >> selfSentMethods [
	^ selfSentMethods
]

{ #category : 'api' }
RTFSelfCallInterpreter >> send: aSelector fromClass: aClass [

	| method |
	selfClass := aClass.
	method := selfClass lookupSelector: aSelector.

	callStack push: (RTFSelfCallRootNode compiledMethod: method).
	method ast acceptVisitor: self.
	rootSelfCall := callStack pop
]

{ #category : 'visiting' }
RTFSelfCallInterpreter >> visitMessageNode: aMessageNode [
	| selfSentMethod callNode |
	aMessageNode receiver acceptVisitor: self.
	aMessageNode arguments do: [ :each | each acceptVisitor: self ].

	(aMessageNode receiver isSelfVariable or: [ aMessageNode receiver isSuperVariable ])
		ifFalse: [ ^ self ].

	selfSentMethod := (aMessageNode receiver isSuperVariable
		ifTrue: [ selfClass superclass ]
		ifFalse: [ selfClass ]) lookupSelector: aMessageNode selector.

	selfSentMethod class = RTFMethodTracer
		ifTrue: [ selfSentMethod := selfSentMethod method ].

	selfSentMethod
		ifNil: [
			Warning signal: ('#{1} can not be found in {2} nor in any super class.' format: { aMessageNode selector . selfClass name }).
			^ self ].

	callNode := RTFSelfCallNode astNode: aMessageNode compiledMethod: selfSentMethod.
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
