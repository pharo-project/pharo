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
		NECInstVarEntry contents: 'a' node: nil.
		NECInstVarEntry contents: 'b' node: nil.
		NECInstVarEntry contents: 'c' node: nil.
		NECInstVarEntry contents: 'supera' node: nil.
		NECInstVarEntry contents: 'superb' node: nil.
		NECInstVarEntry contents: 'superc' node: nil. }
]
