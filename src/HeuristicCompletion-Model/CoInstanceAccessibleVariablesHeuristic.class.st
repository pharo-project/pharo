"
I implement an heuristic for variables accessible from the instance side, such as instance variables and class variables.

See my superclass for details.
"
Class {
	#name : #CoInstanceAccessibleVariablesHeuristic,
	#superclass : #CoASTNodeFetcherHeuristic,
	#category : #'HeuristicCompletion-Model-Heuristics'
}

{ #category : #requests }
CoInstanceAccessibleVariablesHeuristic >> appliesForNode: aNode inContext: aContext [

	^ aContext completionClass notNil
]

{ #category : #requests }
CoInstanceAccessibleVariablesHeuristic >> buildFetcherFor: astNode inContext: completionContext [
	
	| ivFetcher cvFetcher |
	
	"Then instance variables"
	ivFetcher := (CoInstanceVariableFetcher new
		astNode: astNode;
		completionClass: completionContext completionClass) forHierarchy.
	
	"Then class variables if not a workspace"
	cvFetcher :=  (CoClassVariableFetcher new
			astNode: astNode;
			completionClass: completionContext completionClass) forHierarchy.
	
	^ ivFetcher, cvFetcher
]
