"
I implement an heuristic for global variables accessible in the completion environment.

See my superclass for details.
"
Class {
	#name : 'CoGlobalVariablesHeuristic',
	#superclass : 'CoASTNodeFetcherHeuristic',
	#instVars : [
		'globalVariableFetcherClass'
	],
	#category : 'HeuristicCompletion-Model-Heuristics',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Heuristics'
}

{ #category : 'requests' }
CoGlobalVariablesHeuristic >> appliesForNode: aNode inContext: aContext [

	^ aNode isVariable
]

{ #category : 'requests' }
CoGlobalVariablesHeuristic >> buildFetcherFor: aNode inContext: completionContext [
	
	^ self globalVariableFetcherClass new
			completionClass: completionContext completionClass;
			astNode: aNode;
			yourself
]

{ #category : 'requests' }
CoGlobalVariablesHeuristic >> globalVariableFetcherClass [

	^ globalVariableFetcherClass ifNil: [  globalVariableFetcherClass := CoGlobalVariableFetcher ]
]

{ #category : 'requests' }
CoGlobalVariablesHeuristic >> globalVariableFetcherClass: aClass [

	globalVariableFetcherClass := aClass
]
