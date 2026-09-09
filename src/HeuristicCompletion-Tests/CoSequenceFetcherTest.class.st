Class {
	#name : 'CoSequenceFetcherTest',
	#superclass : 'CoBasicFetcherWithElementsTest',
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'running' }
CoSequenceFetcherTest >> setUp [

	super setUp.
	fetcher := (CoInstanceVariableFetcher new
			completionClass: self completionClass)
				, (CoGlobalVariableFetcher new
					systemNavigation: self systemNavigation).
	expectedElements := {
		CoInstVarEntry contents: 'a' node: nil.
		CoInstVarEntry contents: 'b' node: nil.
		CoInstVarEntry contents: 'c' node: nil.
		CoGlobalEntry contents: 'A' node: nil.
		CoGlobalEntry contents: 'B' node: nil.
		CoGlobalEntry contents: 'C' node: nil. }
]

{ #category : 'tests' }
CoSequenceFetcherTest >> testCompletionCompositionNextManyShouldStopAfterEndOfElements [

	self assertCollectionOfEntry: (self fetcher next: 10) equals: expectedElements
]

{ #category : 'tests' }
CoSequenceFetcherTest >> testCompletionCompositionNextShouldReturnValuesOfSecondCompletionAfterFirst [

	"Consume instance variables"
	self fetcher next: 3.
	self assertEntry: self fetcher next equals: (CoGlobalEntry contents: 'A' node: nil)
]

{ #category : 'tests' }
CoSequenceFetcherTest >> testNextManyAfterFirstSubfetcherShouldReturnCollectionWithSecondSubfetcherElements [

	self
		assertCollectionOfEntry: (self fetcher next: 5)
		equals: expectedElements allButLast
]
