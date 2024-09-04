"
I am an abstract class for heuristics for messages sent to variables.

See my superclass and concrete subclasses for details.
"
Class {
	#name : 'CoVariableMessageHeuristic',
	#superclass : 'CoASTNodeFetcherHeuristic',
	#category : 'HeuristicCompletion-Model-Heuristics',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Heuristics'
}

{ #category : 'testing' }
CoVariableMessageHeuristic class >> isAbstract [

	^ self == CoVariableMessageHeuristic
]

{ #category : 'requests' }
CoVariableMessageHeuristic >> appliesForNode: aMessageNode inContext: aContext [

	^ aMessageNode receiver isVariable
]
