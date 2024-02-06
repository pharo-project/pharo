Class {
	#name : 'ReNotInCascadedMessageCondition',
	#superclass : 'ReSubtreeCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
ReNotInCascadedMessageCondition >> check [

	^ subtree parent isCascade not
]

{ #category : 'displaying' }
ReNotInCascadedMessageCondition >> violationMessageOn: aStream [ 

	^ aStream nextPutAll: 'Cannot extract code in a cascaded message.'
]
