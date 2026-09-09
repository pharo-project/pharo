Class {
	#name : 'CoClassVariableFetcherTest',
	#superclass : 'CoBasicFetcherWithElementsTest',
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'running' }
CoClassVariableFetcherTest >> setUp [

	super setUp.
	fetcher := CoClassVariableFetcher new.
	fetcher completionClass: self completionClass.
	expectedElements := {
		CoClassVarEntry contents: 'Ca' node: nil.
		CoClassVarEntry contents: 'Cb' node: nil.
		CoClassVarEntry contents: 'Cc' node: nil.}
]
