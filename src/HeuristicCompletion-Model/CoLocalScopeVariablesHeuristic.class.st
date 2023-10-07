"
I implement an heuristic for local variables accessible from the current AST node.

See my superclass for details.
"
Class {
	#name : 'CoLocalScopeVariablesHeuristic',
	#superclass : 'CoASTNodeFetcherHeuristic',
	#category : 'HeuristicCompletion-Model-Heuristics',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Heuristics'
}

{ #category : 'requests' }
CoLocalScopeVariablesHeuristic >> appliesForNode: aNode inContext: aContext [

	^ true
]

{ #category : 'requests' }
CoLocalScopeVariablesHeuristic >> buildFetcherFor: aNode inContext: aContext [

	^ CoMethodVariableFetcher new completionASTNode: aNode
]
