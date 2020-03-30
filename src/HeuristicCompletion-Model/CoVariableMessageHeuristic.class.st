"
I am an abstract class for heuristics for messages sent to variables.

See my superclass and concrete subclasses for details.
"
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
