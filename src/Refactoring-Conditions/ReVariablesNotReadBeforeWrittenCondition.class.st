"
I am a condition that checks if the variables I get from the receiver are read before written.

By default I will not check for temporaries variables, it's configurable using the **checkForTemporaryVariables:** method.
"
Class {
	#name : 'ReVariablesNotReadBeforeWrittenCondition',
	#superclass : 'ReSubtreeCondition',
	#instVars : [
		'variables',
		'checkForTemporaryVariables'
	],
	#category : 'Refactoring-Conditions',
	#package : 'Refactoring-Conditions'
}

{ #category : 'checking' }
ReVariablesNotReadBeforeWrittenCondition >> check [

	(subtree isNil or: [ variables isNil ]) ifTrue: [ ^ false ].
	^ (OCReadBeforeWrittenTester new
		   checkNewTemps: checkForTemporaryVariables;
		   initializeVars: variables;
		   executeTree: subtree;
		   read) isEmpty
]

{ #category : 'accessing' }
ReVariablesNotReadBeforeWrittenCondition >> checkForTemporaryVariables: aBoolean [

	checkForTemporaryVariables := aBoolean
]

{ #category : 'initialization' }
ReVariablesNotReadBeforeWrittenCondition >> initialize [

	super initialize.
	checkForTemporaryVariables := false
]

{ #category : 'initialization' }
ReVariablesNotReadBeforeWrittenCondition >> variables: aCollection [ 
	
	variables := aCollection 
]

{ #category : 'displaying' }
ReVariablesNotReadBeforeWrittenCondition >> violationMessageOn: aStream [

	self check ifTrue: [ ^ self ].
	aStream
		nextPutAll: 'Cannot extract selected code because variables: ';
		nextPutAll: variables asString;
		nextPutAll: ' are read before written.'
]
