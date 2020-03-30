Class {
	#name : #CoInstanceAccessibleVariablesHeuristic,
	#superclass : #CoASTNodeFetcherHeuristic,
	#category : #'HeuristicCompletion-Model-Heuristics'
}

{ #category : #requests }
CoInstanceAccessibleVariablesHeuristic >> appliesForNode: aNode inContext: aContext [

	^ aContext isWorkspace not
]

{ #category : #requests }
CoInstanceAccessibleVariablesHeuristic >> buildFetcherFor: aNode inContext: completionContext [
	
	| ivFetcher cvFetcher |
	
	"Then instance variables"
	ivFetcher := (CoInstanceVariableFetcher new
		completionClass: completionContext completionClass) forHierarchy.
	
	"Then class variables if not a workspace"
	cvFetcher :=  (CoClassVariableFetcher new
			completionClass: completionContext completionClass) forHierarchy.
	
	^ ivFetcher, cvFetcher
]
