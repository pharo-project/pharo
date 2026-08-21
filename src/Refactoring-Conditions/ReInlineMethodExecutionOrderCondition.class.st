"
I check whether inlining would require moving statements before the
original message send, potentially changing the execution order.

I need the sourceMessage node and the inlineParseTree of the method
to be inlined.
"
Class {
	#name : 'ReInlineMethodExecutionOrderCondition',
	#superclass : 'ReMethodsCondition',
	#instVars : [
		'sourceMessage',
		'inlineParseTree'
	],
	#category : 'Refactoring-Conditions',
	#package : 'Refactoring-Conditions'
}

{ #category : 'instance creation' }
ReInlineMethodExecutionOrderCondition >> sourceMessage: aMessageNode inlineParseTree: aParseTree [

	sourceMessage := aMessageNode.
	inlineParseTree := aParseTree
]

{ #category : 'displaying' }
ReInlineMethodExecutionOrderCondition >> violationMessageOn: aStream [

	aStream
		nextPutAll: 'To inline this method, we need to move some of its statements before the original message send.';
		nextPutAll: 'This could change the order of execution, which can change the behavior.'
]

{ #category : 'accessing' }
ReInlineMethodExecutionOrderCondition >> violators [

	violators ifNotNil: [ ^ violators ].
	violators := OrderedCollection new.
	(inlineParseTree body statements size > 1 and: [ sourceMessage isEvaluatedFirst not ]) ifTrue: [ violators add: sourceMessage ].
	^ violators
]
