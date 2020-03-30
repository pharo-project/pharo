Class {
	#name : #CoLocalScopeVariablesHeuristic,
	#superclass : #CoASTNodeFetcherHeuristic,
	#category : #'HeuristicCompletion-Model-Heuristics'
}

{ #category : #requests }
CoLocalScopeVariablesHeuristic >> appliesForNode: aNode inContext: aContext [

	^ true
]

{ #category : #requests }
CoLocalScopeVariablesHeuristic >> buildFetcherFor: aNode inContext: aContext [

	^ CoMethodVariableFetcher new completionASTNode: aNode
]
