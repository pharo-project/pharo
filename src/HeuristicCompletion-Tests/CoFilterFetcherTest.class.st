Class {
	#name : 'CoFilterFetcherTest',
	#superclass : 'CoBasicFetcherWithElementsTest',
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'running' }
CoFilterFetcherTest >> setUp [

	super setUp.
	fetcher := CoInstanceVariableFetcher new.
	fetcher completionClass: self completionClass.
	fetcher filter: (CoCaseSensitiveBeginsWithFilter filterString: 'a') negated.
	expectedElements := {
		CoInstVarEntry contents: 'b' node: nil.
		CoInstVarEntry contents: 'c' node: nil. }
]
