"
I am a RBRefactoring intended for prepagating another refactoring. We call to propagate a refactoring to redo just the secondary effects of such refactoring. 

For example, the propagation of a 'message rename' is to change the senders of the old selector to use the new selector. 
"
Class {
	#name : #EpRBPropagateRefactoring,
	#superclass : #RBRefactoring,
	#instVars : [
		'targetRefactoring'
	],
	#category : #'Epicea-Support'
}

{ #category : #'instance creation' }
EpRBPropagateRefactoring class >> target: aRefactoring [ 
	^ self basicNew 
		initializeWith: aRefactoring;
		yourself
]

{ #category : #converting }
EpRBPropagateRefactoring >> asEpiceaEvent [
	^ EpPropagateRefactoring target: targetRefactoring asEpiceaEvent
]

{ #category : #initialization }
EpRBPropagateRefactoring >> initializeWith: aRefactoring [ 
	self initialize.
	targetRefactoring := aRefactoring 
]

{ #category : #private }
EpRBPropagateRefactoring >> primitiveExecute [
	targetRefactoring propagateTransformation.
]

{ #category : #accessing }
EpRBPropagateRefactoring >> setOption: aString toUse: aBlockClosure [ 
	targetRefactoring setOption: aString toUse: aBlockClosure 
]

{ #category : #accessing }
EpRBPropagateRefactoring >> whatToDisplayIn: aNautilusChangesBrowser [ 
	^ targetRefactoring whatToDisplayIn: aNautilusChangesBrowser
]
