Class {
	#name : 'ReSubtreeCondition',
	#superclass : 'ReReifiedCondition',
	#instVars : [
		'subtree'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'testing' }
ReSubtreeCondition class >> isAbstract [

	^ self == ReSubtreeCondition
]

{ #category : 'checking' }
ReSubtreeCondition >> check [

	^ self subclassResponsibility
]

{ #category : 'accessing' }
ReSubtreeCondition >> subtree: aRBMethodNode [

	subtree := aRBMethodNode
]
