Class {
	#name : 'CoHierarchyClassVariableFetcherTest',
	#superclass : 'CoClassVariableFetcherTest',
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'running' }
CoHierarchyClassVariableFetcherTest >> setUp [

	super setUp.
	fetcher := CoClassVariableFetcher new
		completionClass: self completionClass;
		forHierarchy.
	expectedElements := {
		CoClassVarEntry contents: 'Ca' node: nil.
		CoClassVarEntry contents: 'Cb' node: nil.
		CoClassVarEntry contents: 'Cc' node: nil.
		CoClassVarEntry contents: 'SuperCa' node: nil.
		CoClassVarEntry contents: 'SuperCb' node: nil.
		CoClassVarEntry contents: 'SuperCc' node: nil.}
]
