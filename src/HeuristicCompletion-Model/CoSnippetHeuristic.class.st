"
I'm a heuristic for snippet fetcher. Users are able to use short string and get back a long snippet of code.
"
Class {
	#name : 'CoSnippetHeuristic',
	#superclass : 'CoASTNodeFetcherHeuristic',
	#category : 'HeuristicCompletion-Model-NewFetchers',
	#package : 'HeuristicCompletion-Model',
	#tag : 'NewFetchers'
}

{ #category : 'requests' }
CoSnippetHeuristic >> appliesForNode: aNode inContext: aContext [
	"People may want to get their expanded snippet in many different places so for now we do not restraint the place."
	
	^ true
]

{ #category : 'requests' }
CoSnippetHeuristic >> buildFetcherFor: aMessageNode inContext: aContext [

	^ CoSnippetFetcher new 
]
