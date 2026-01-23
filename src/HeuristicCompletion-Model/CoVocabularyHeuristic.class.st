Class {
	#name : 'CoVocabularyHeuristic',
	#superclass : 'CoASTNodeFetcherHeuristic',
	#category : 'HeuristicCompletion-Model-NewFetchers',
	#package : 'HeuristicCompletion-Model',
	#tag : 'NewFetchers'
}

{ #category : 'requests' }
CoVocabularyHeuristic >> appliesForNode: aNode inContext: aContext [

	^ aNode isVariable or: [ 
		aNode isAssignment or: [ 
			aNode isArgumentVariable ] ]
	
]

{ #category : 'requests' }
CoVocabularyHeuristic >> buildFetcherFor: aMessageNode inContext: aContext [

	^ CoVocabularyFetcher new 
]
