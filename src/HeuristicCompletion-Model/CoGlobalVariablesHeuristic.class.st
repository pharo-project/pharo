Class {
	#name : #CoGlobalVariablesHeuristic,
	#superclass : #CoASTNodeFetcherHeuristic,
	#category : #'HeuristicCompletion-Model-Heuristics'
}

{ #category : #requests }
CoGlobalVariablesHeuristic >> appliesForNode: aNode inContext: aContext [

	^ aNode isVariable and: [ aNode name first isUppercase ]
]

{ #category : #requests }
CoGlobalVariablesHeuristic >> buildFetcherFor: aNode inContext: completionContext [

	^ CoGlobalVariableFetcher new
			completionEnvironment: completionContext completionEnvironment;
			yourself
]
