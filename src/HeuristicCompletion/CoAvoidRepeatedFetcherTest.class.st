Class {
	#name : #CoAvoidRepeatedFetcherTest,
	#superclass : #CoBasicFetcherWithElementsTest,
	#category : #'HeuristicCompletion-Tests'
}

{ #category : #running }
CoAvoidRepeatedFetcherTest >> setUp [

	super setUp.
	fetcher := CoAvoidRepeatedFetcher new
		decoree: (CoCollectionFetcher onCollection: #( a b b a c )).
	expectedElements := #(a b c).
]
