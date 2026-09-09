Class {
	#name : 'CoSharedPoolVariableFetcherTest',
	#superclass : 'CoBasicFetcherWithElementsTest',
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'running' }
CoSharedPoolVariableFetcherTest >> setUp [

	super setUp.
	fetcher := CoSharedPoolVariableFetcher new.
	fetcher completionClass: self completionClass.
	fetcher := fetcher forHierarchy.
	expectedElements := {
		CoClassVarEntry contents: 'Sa' node: nil.
		CoClassVarEntry contents: 'Sb' node: nil.
		CoClassVarEntry contents: 'SSupera' node: nil.
		CoClassVarEntry contents: 'SSuperb' node: nil.
		CoClassVarEntry contents: 'Sc' node: nil.
		CoClassVarEntry contents: 'SSuperc' node: nil.}
]
