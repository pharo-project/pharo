Class {
	#name : 'ReParsableSubtreeCondition',
	#superclass : 'ReSubtreeCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
ReParsableSubtreeCondition >> check [ 
	"TODO: Does it make sense here to check if subtree is of correct type?"
	
	^ subtree isNotNil
]

{ #category : 'displaying' }
ReParsableSubtreeCondition >> violationMessageOn: aStream [

	aStream nextPutAll: 'Cannot extract selected code, it is not a valid subtree.'
]
