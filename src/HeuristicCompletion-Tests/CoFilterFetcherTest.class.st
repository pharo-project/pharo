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
		NECInstVarEntry contents: 'b' node: nil.
		NECInstVarEntry contents: 'c' node: nil. }
]
