Class {
	#name : 'CoSnippetHeuristic',
	#superclass : 'CoASTNodeFetcherHeuristic',
	#category : 'HeuristicCompletion-Model-NewFetchers',
	#package : 'HeuristicCompletion-Model',
	#tag : 'NewFetchers'
}

{ #category : 'requests' }
CoSnippetHeuristic >> appliesForNode: aNode inContext: aContext [

	^ aNode isVariable or: [ 
		aNode isAssignment or: [ 
			aNode isArgumentVariable ] ]
	
]

{ #category : 'requests' }
CoSnippetHeuristic >> buildFetcherFor: aMessageNode inContext: aContext [

	^ CoSnippetFetcher new 
]
