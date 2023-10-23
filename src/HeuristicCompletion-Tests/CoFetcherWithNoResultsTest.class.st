Class {
	#name : 'CoFetcherWithNoResultsTest',
	#superclass : 'CoBasicFetcherTest',
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'running' }
CoFetcherWithNoResultsTest >> setUp [

	super setUp.
	fetcher := CoInstanceVariableFetcher new.
	fetcher completionClass: self completionClass.
	fetcher := fetcher filter: (CoCaseSensitiveBeginsWithFilter filterString: '*Non¨_exiuting')
]

{ #category : 'tests' }
CoFetcherWithNoResultsTest >> testCompletionWithNoResultsShouldBeAtEnd [

	self assert: self fetcher atEnd
]
