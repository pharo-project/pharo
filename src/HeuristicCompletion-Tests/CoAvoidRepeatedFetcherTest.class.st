Class {
	#name : 'CoAvoidRepeatedFetcherTest',
	#superclass : 'CoBasicFetcherWithElementsTest',
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'running' }
CoAvoidRepeatedFetcherTest >> setUp [

	super setUp.
	fetcher := CoAvoidRepeatedFetcher new
		decoree: (CoCollectionFetcher onCollection: #( a b b a c )).
	expectedElements := #(a b c)
]
