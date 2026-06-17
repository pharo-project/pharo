Class {
	#name : 'ReNotInCascadedMessageCondition',
	#superclass : 'ReSubtreeCondition',
	#category : 'Refactoring-Conditions',
	#package : 'Refactoring-Conditions'
}

{ #category : 'checking' }
ReNotInCascadedMessageCondition >> check [

	subtree ifNil: [ ^ false ].
	^ subtree parent isCascade not
]

{ #category : 'displaying' }
ReNotInCascadedMessageCondition >> violationMessageOn: aStream [ 

	^ aStream nextPutAll: 'Cannot extract code in a cascaded message.'
]
