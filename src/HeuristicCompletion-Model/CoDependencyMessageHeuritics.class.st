"
I'm a heurisitc that apply when i see a dependency for a package
"
Class {
	#name : 'CoDependencyMessageHeuritics',
	#superclass : 'CoASTNodeFetcherHeuristic',
	#category : 'HeuristicCompletion-Model-Heuristics',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Heuristics'
}

{ #category : 'requests' }
CoDependencyMessageHeuritics >> appliesForNode: aNode inContext: aContext [
    ^ (aNode isMessage and: [ aNode receiver isVariable ])
        or: [ aNode isMethod ]
]

{ #category : 'requests' }
CoDependencyMessageHeuritics >> buildFetcherFor: aNode inContext: completionContext [

	^ CoPackageScopedSelectorFetcher new
		  completionClass: completionContext completionClass;
		  astNode: aNode;
		  yourself
]
