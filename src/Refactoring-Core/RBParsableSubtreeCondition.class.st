Class {
	#name : 'RBParsableSubtreeCondition',
	#superclass : 'RBSubtreeCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
RBParsableSubtreeCondition >> check [ 
	"TODO: Does it make sense here to check if subtree is of correct type?"
	
	^ subtree isNotNil
]

{ #category : 'displaying' }
RBParsableSubtreeCondition >> violationMessageOn: aStream [

	^ 'Cannot extract selected code, it is not a valid subtree.'
]
