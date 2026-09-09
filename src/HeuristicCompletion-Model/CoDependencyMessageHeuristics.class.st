"
I'm a heurisitc that apply when i see a dependency for a package
"
Class {
	#name : 'CoDependencyMessageHeuristics',
	#superclass : 'CoASTNodeFetcherHeuristic',
	#category : 'HeuristicCompletion-Model-Heuristics',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Heuristics'
}

{ #category : 'requests' }
CoDependencyMessageHeuristics >> appliesForNode: aNode inContext: aContext [
    ^ (aNode isMessage and: [ aNode receiver isVariable ])
        or: [ aNode isMethod ]
]

{ #category : 'requests' }
CoDependencyMessageHeuristics >> buildFetcherFor: aNode inContext: completionContext [

	^ CoDependencySelectorFetcher new
		  completionClass: completionContext completionClass;
		  astNode: aNode;
		  yourself
]
