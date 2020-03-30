Class {
	#name : #CoVariableMessageHeuristic,
	#superclass : #CoASTNodeFetcherHeuristic,
	#category : #'HeuristicCompletion-Model-Heuristics'
}

{ #category : #testing }
CoVariableMessageHeuristic class >> isAbstract [

	^ self == CoVariableMessageHeuristic
]

{ #category : #requests }
CoVariableMessageHeuristic >> appliesForNode: aRBMessageNode inContext: aContext [

	^ aRBMessageNode receiver isVariable
]
