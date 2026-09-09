Class {
	#name : 'CoHierarchyInstanceVariableFetcherTest',
	#superclass : 'CoInstanceVariableFetcherTest',
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'running' }
CoHierarchyInstanceVariableFetcherTest >> setUp [

	super setUp.
	fetcher := CoInstanceVariableFetcher new
		completionClass: self completionClass;
		forHierarchy.
	expectedElements := {
		CoInstVarEntry contents: 'a' node: nil.
		CoInstVarEntry contents: 'b' node: nil.
		CoInstVarEntry contents: 'c' node: nil.
		CoInstVarEntry contents: 'supera' node: nil.
		CoInstVarEntry contents: 'superb' node: nil.
		CoInstVarEntry contents: 'superc' node: nil. }
]
