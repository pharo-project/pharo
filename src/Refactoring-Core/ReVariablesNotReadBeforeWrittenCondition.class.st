Class {
	#name : 'ReVariablesNotReadBeforeWrittenCondition',
	#superclass : 'ReSubtreeCondition',
	#instVars : [
		'variables'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
ReVariablesNotReadBeforeWrittenCondition >> check [

	(subtree isNil or: [ variables isNil ]) ifTrue: [ ^ false ].
	
	^ (OCReadBeforeWrittenTester
		   readBeforeWritten: variables
		   in: subtree) isEmpty
]

{ #category : 'initialization' }
ReVariablesNotReadBeforeWrittenCondition >> variables: aCollection [ 
	
	variables := aCollection 
]

{ #category : 'displaying' }
ReVariablesNotReadBeforeWrittenCondition >> violationMessageOn: aStream [

	aStream
		nextPutAll: 'Cannot extract selected code because variables: ';
		nextPutAll: variables asString;
		nextPutAll: ' are read before written.'
]
