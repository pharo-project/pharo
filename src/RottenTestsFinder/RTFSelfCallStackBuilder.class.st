"
I build a selfcall tree from a class and a selector.

The self call tree is as big as possible. That is to say, if calls are made in conditional blocks, they are taken in account independently from the value of the conditional.

See: the method #example.
"
Class {
	#name : 'RTFSelfCallStackBuilder',
	#superclass : 'RTFSelfCallVisitorInterpreter',
	#instVars : [
		'callStack',
		'rootSelfCall'
	],
	#category : 'RottenTestsFinder-SelfInterpreter',
	#package : 'RottenTestsFinder',
	#tag : 'SelfInterpreter'
}

{ #category : 'example' }
RTFSelfCallStackBuilder class >> example [
	^ RTFSelfCallStackBuilder new
		considerClassesThat: [ :class | class inheritsFrom: Collection ];
		send: #select: fromClass: OrderedCollection;
		rootSelfCall
]

{ #category : 'private' }
RTFSelfCallStackBuilder >> analyze [

	super analyze.
	callStack push: callStack top subCalls last.
	selfSentMethod ast acceptVisitor: self.
	callStack pop
]

{ #category : 'initialization' }
RTFSelfCallStackBuilder >> initialize [ 

	super initialize.
	callStack := Stack new
]

{ #category : 'private' }
RTFSelfCallStackBuilder >> lookupMethodFromNode: aMessageNode [

	super lookupMethodFromNode: aMessageNode.
	selfSentMethod class = RTFMethodTracer ifTrue: [ selfSentMethod := selfSentMethod method ]
]

{ #category : 'private' }
RTFSelfCallStackBuilder >> preAnalysisHook: aMessageNode [

	| callNode |
	callNode := RTFSelfCallNode astNode: aMessageNode compiledMethod: selfSentMethod.
	callStack top addCall: callNode.
]

{ #category : 'accessing' }
RTFSelfCallStackBuilder >> rootSelfCall [
	^ rootSelfCall
]

{ #category : 'accessing' }
RTFSelfCallStackBuilder >> rootSelfCall: anObject [
	rootSelfCall := anObject
]

{ #category : 'api' }
RTFSelfCallStackBuilder >> send: aSelector fromClass: aClass [

	| method |
	selfClass := aClass.
	method := selfClass lookupSelector: aSelector.

	callStack push: (RTFSelfCallRootNode compiledMethod: method).
	method ast acceptVisitor: self.
	rootSelfCall := callStack pop
]
