Class {
	#name : 'CoMethodVariablesFetcherTest',
	#superclass : 'CoBasicFetcherWithElementsTest',
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'running' }
CoMethodVariablesFetcherTest >> setUp [

	super setUp.
	fetcher := CoMethodVariableFetcher new.
	fetcher completionASTNode: self innerASTNode.
	expectedElements := #(blockTemporary blockArgument methodTemporary methodArgument)
		collect: [:e | NECLocalEntry contents: e node: nil ]
]
