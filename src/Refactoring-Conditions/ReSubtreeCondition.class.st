Class {
	#name : 'ReSubtreeCondition',
	#superclass : 'ReReifiedCondition',
	#instVars : [
		'subtree'
	],
	#category : 'Refactoring-Conditions',
	#package : 'Refactoring-Conditions'
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
ReSubtreeCondition >> subtree: aMethodNode [

	subtree := aMethodNode
]
