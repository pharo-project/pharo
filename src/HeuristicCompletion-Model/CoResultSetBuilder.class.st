Class {
	#name : #CoResultSetBuilder,
	#superclass : #Object,
	#instVars : [
		'completionContext'
	],
	#category : #'HeuristicCompletion-Model-SystemIntegration'
}

{ #category : #'instance creation' }
CoResultSetBuilder class >> initializeOnContext: aSystemCompletionContext [
	
	^ self new
		initializeOnContext: aSystemCompletionContext;
		yourself
]

{ #category : #'API - building' }
CoResultSetBuilder >> buildCompletion [

	self subclassResponsibility
]

{ #category : #accessing }
CoResultSetBuilder >> completionContext [
	^ completionContext
]

{ #category : #accessing }
CoResultSetBuilder >> completionContext: anObject [
	completionContext := anObject
]

{ #category : #initialization }
CoResultSetBuilder >> initializeOnContext: aCompletionContext [ 
	
	completionContext := aCompletionContext
]
