Class {
	#name : 'RBVariablesNotReadBeforeWrittenCondition',
	#superclass : 'RBSubtreeCondition',
	#instVars : [
		'variables'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
RBVariablesNotReadBeforeWrittenCondition >> check [

	(subtree isNil or: [ variables isNil ]) ifTrue: [ ^ false ].
	
	^ (RBReadBeforeWrittenTester
		   readBeforeWritten: variables
		   in: subtree) isEmpty
]

{ #category : 'initiali' }
RBVariablesNotReadBeforeWrittenCondition >> variables: aCollection [ 
	
	variables := aCollection 
]

{ #category : 'displaying' }
RBVariablesNotReadBeforeWrittenCondition >> violationMessageOn: aStream [

	aStream
		nextPutAll: 'Cannot extract selected code because variables: ';
		nextPutAll: variables asString;
		nextPutAll: ' are read before written.'
]
