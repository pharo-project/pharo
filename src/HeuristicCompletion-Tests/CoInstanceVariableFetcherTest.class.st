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
		NECInstVarEntry contents: 'a' node: nil.
		NECInstVarEntry contents: 'b' node: nil.
		NECInstVarEntry contents: 'c' node: nil. }
]
