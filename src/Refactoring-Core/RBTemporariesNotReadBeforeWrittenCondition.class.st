Class {
	#name : 'RBTemporariesNotReadBeforeWrittenCondition',
	#superclass : 'RBCondition',
	#instVars : [
		'subtree',
		'temporaries'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
RBTemporariesNotReadBeforeWrittenCondition >> check [

	(subtree isNil or: [ temporaries isNil ]) ifTrue: [ ^ false ].
	
	^ (RBReadBeforeWrittenTester
		   readBeforeWritten: temporaries
		   in: subtree) isEmpty
]

{ #category : 'initiali' }
RBTemporariesNotReadBeforeWrittenCondition >> subtree: aRBMessageNode temporaries: aCollection [ 
	
	subtree := aRBMessageNode.
	temporaries := aCollection 
]

{ #category : 'displaying' }
RBTemporariesNotReadBeforeWrittenCondition >> violationMessageOn: aStream [

	^ 'Cannot extract temporaries because they are read before written.'
]
