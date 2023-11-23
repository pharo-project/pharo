Class {
	#name : 'CoGlobalVariableFetcherTest',
	#superclass : 'CoBasicFetcherWithElementsTest',
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'running' }
CoGlobalVariableFetcherTest >> setUp [

	super setUp.
	fetcher := CoGlobalVariableFetcher new.
	fetcher systemNavigation: self systemNavigation.

	expectedElements := {
		NECGlobalEntry contents: 'A' node: nil.
		NECGlobalEntry contents: 'B' node: nil.
		NECGlobalEntry contents: 'C' node: nil. }
]
