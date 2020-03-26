Class {
	#name : #CoFetcherWithNoResultsTest,
	#superclass : #CoBasicFetcherTest,
	#category : #'HeuristicCompletion-Tests-Core'
}

{ #category : #running }
CoFetcherWithNoResultsTest >> setUp [

	super setUp.
	fetcher := CoInstanceVariableFetcher new.
	fetcher completionClass: self completionClass.
	fetcher := fetcher select: [ :e | false ].
]

{ #category : #tests }
CoFetcherWithNoResultsTest >> testCompletionWithNoResultsShouldBeAtEnd [

	self assert: self fetcher atEnd
]
