Class {
	#name : #CoCompletionEngine,
	#superclass : #CompletionEngine,
	#instVars : [
		'completionBuilder'
	],
	#category : #'HeuristicCompletion-Morphic'
}

{ #category : #accessing }
CoCompletionEngine >> completionBuilder [

	^ completionBuilder ifNil: [ CoASTHeuristicsResultSetBuilder new ]
]

{ #category : #accessing }
CoCompletionEngine >> completionBuilder: anObject [
	completionBuilder := anObject
]

{ #category : #'menu morph' }
CoCompletionEngine >> createContext [

	^ super createContext
		completionBuilder: self completionBuilder;
		yourself
]

{ #category : #initialization }
CoCompletionEngine >> initialize [

	super initialize.
	contextClass := CoCompletionContext.
]
