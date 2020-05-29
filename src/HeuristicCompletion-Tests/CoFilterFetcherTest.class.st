Class {
	#name : #CoFilterFetcherTest,
	#superclass : #CoBasicFetcherWithElementsTest,
	#category : #'HeuristicCompletion-Tests-Core'
}

{ #category : #running }
CoFilterFetcherTest >> setUp [

	super setUp.
	fetcher := CoInstanceVariableFetcher new.
	fetcher completionClass: self completionClass.
	fetcher := fetcher select: [ :e | (e contents beginsWith: 'a') not ].
	expectedElements := { 
		NECInstVarEntry contents: 'b' node: nil. 
		NECInstVarEntry contents: 'c' node: nil. }
]
