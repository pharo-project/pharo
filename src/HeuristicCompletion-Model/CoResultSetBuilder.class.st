"
I am the an AST based heuristics builder.
The completion engine calls configures me with a completion context and eventually sends me the #buildCompletion message.
I then return a Complishon object configured with a correct fetcher and initial filter.

My subclasses define how to create the complishon given the context.
"
Class {
	#name : 'CoResultSetBuilder',
	#superclass : 'Object',
	#instVars : [
		'completionContext'
	],
	#category : 'HeuristicCompletion-Model-SystemIntegration',
	#package : 'HeuristicCompletion-Model',
	#tag : 'SystemIntegration'
}

{ #category : 'instance creation' }
CoResultSetBuilder class >> initializeOnContext: aSystemCompletionContext [

	^ self new
		initializeOnContext: aSystemCompletionContext;
		yourself
]

{ #category : 'API - building' }
CoResultSetBuilder >> buildCompletion [

	self subclassResponsibility
]

{ #category : 'accessing' }
CoResultSetBuilder >> completionContext [
	^ completionContext
]

{ #category : 'accessing' }
CoResultSetBuilder >> completionContext: anObject [
	completionContext := anObject
]

{ #category : 'initialization' }
CoResultSetBuilder >> initializeOnContext: aCompletionContext [

	completionContext := aCompletionContext
]
