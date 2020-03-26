"
I am an adaptor between the Morphic text morphs supporting completion and Complishon.
I contain a complishonBuilder that is used to create a complishon.

See my superclass for details.
"
Class {
	#name : #CoCompletionEngine,
	#superclass : #CompletionEngine,
	#instVars : [
		'completionBuilder'
	],
	#category : #'HeuristicCompletion-SystemIntegration'
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
