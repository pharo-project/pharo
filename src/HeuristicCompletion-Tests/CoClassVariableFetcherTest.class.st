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
		NECClassVarEntry contents: 'Ca' node: nil.
		NECClassVarEntry contents: 'Cb' node: nil.
		NECClassVarEntry contents: 'Cc' node: nil.}
]
