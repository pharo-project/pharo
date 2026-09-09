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
		CoGlobalEntry contents: 'A' node: nil.
		CoGlobalEntry contents: 'B' node: nil.
		CoGlobalEntry contents: 'C' node: nil. }
]
