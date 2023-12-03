Class {
	#name : 'RBSubtreeCondition',
	#superclass : 'RBCondition',
	#instVars : [
		'subtree'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'testing' }
RBSubtreeCondition class >> isAbstract [

	^ self == RBSubtreeCondition
]

{ #category : 'checking' }
RBSubtreeCondition >> check [

	^ self subclassResponsibility
]

{ #category : 'accessing' }
RBSubtreeCondition >> subtree: aRBMethodNode [

	subtree := aRBMethodNode
]

{ #category : 'displaying' }
RBSubtreeCondition >> violationMessageOn: aStream [ 
	^ self subclassResponsibility
]
