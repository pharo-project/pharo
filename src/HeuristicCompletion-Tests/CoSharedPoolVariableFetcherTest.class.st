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
		NECClassVarEntry contents: 'Sa' node: nil.
		NECClassVarEntry contents: 'Sb' node: nil.
		NECClassVarEntry contents: 'SSupera' node: nil.
		NECClassVarEntry contents: 'SSuperb' node: nil.
		NECClassVarEntry contents: 'Sc' node: nil.
		NECClassVarEntry contents: 'SSuperc' node: nil.}
]
