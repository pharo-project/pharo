"
I implement an heuristic for variables in a workspace/playground.

See my superclass for details.
"
Class {
	#name : #CoWorkspaceVariablesHeuristic,
	#superclass : #CoASTNodeFetcherHeuristic,
	#category : #'HeuristicCompletion-Model-Heuristics'
}

{ #category : #requests }
CoWorkspaceVariablesHeuristic >> appliesForNode: aNode inContext: aContext [

	^ aContext isWorkspace
]

{ #category : #requests }
CoWorkspaceVariablesHeuristic >> buildFetcherFor: aNode inContext: aContext [

	^ (CoCollectionFetcher onCollection: aContext variableBindingNames)
		collect: [ :e | NECLocalEntry contents: e node: aNode ]
]
