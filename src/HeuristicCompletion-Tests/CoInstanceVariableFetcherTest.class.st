Class {
	#name : 'CoInstanceVariableFetcherTest',
	#superclass : 'CoBasicFetcherWithElementsTest',
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'running' }
CoInstanceVariableFetcherTest >> setUp [

	super setUp.
	fetcher := CoInstanceVariableFetcher new.
	fetcher completionClass: self completionClass.
	expectedElements := {
		CoInstVarEntry contents: 'a' node: nil.
		CoInstVarEntry contents: 'b' node: nil.
		CoInstVarEntry contents: 'c' node: nil. }
]
