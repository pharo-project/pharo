Class {
	#name : 'CoDependencyPackageScopedGlobalVariableTest',
	#superclass : 'CoMiniTestCase',
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'tests' }
CoDependencyPackageScopedGlobalVariableTest >> testCompletionClassIsUnknownShouldGiveZero [

	| localFetcher fetchedElements |
	localFetcher := CoDependencyGlobalVariableFetcher  new.
	localFetcher
		filter: (CoBeginsWithFilter caseSensitive: false filterString: 'Obj').

	fetchedElements := localFetcher next: 1.

	"since the global namespace is not sorted by any means I have no warranty that the next global is the one
	I asked for so "
	self assert: (fetchedElements collect: #contents) isEmpty.
]

{ #category : 'tests' }
CoDependencyPackageScopedGlobalVariableTest >> testLocalFirstThenDependent [
	| localFetcher  fetchedElements |
	localFetcher := CoDependencyGlobalVariableFetcher  new.
	localFetcher
		completionClass: ClassWithDependencyOnData2;
		filter: (CoBeginsWithFilter caseSensitive: false filterString: 'Diff').

	fetchedElements := localFetcher next: 3.

	self assert: fetchedElements size equals: 3.
	self
		assert: (fetchedElements collect: #contents) asArray
		equals: #( 'DifferentAnalyserData4' 'DifferentAnalyserData1' 'DifferentAnalyserData2')
]

{ #category : 'tests' }
CoDependencyPackageScopedGlobalVariableTest >> testNoMatchEverywhereDoesNotReturnResult [
	| localFetcher  fetchedElements |
	localFetcher := CoDependencyGlobalVariableFetcher  new.
	localFetcher
		completionClass: ClassWithDependencyOnData2;
		filter: (CoBeginsWithFilter caseSensitive: false filterString: 'XXFG').

	fetchedElements := localFetcher next: 1.

	self assert: fetchedElements isEmpty
	
]

{ #category : 'tests' }
CoDependencyPackageScopedGlobalVariableTest >> testWhenNoLocalAndNoDependentThenGlobal [

	| localFetcher  fetchedElements |
	localFetcher := CoDependencyGlobalVariableFetcher  new.
	localFetcher
		completionClass: ClassWithDependencyOnData2;
		filter: (CoBeginsWithFilter caseSensitive: false filterString: 'Po').

	fetchedElements := localFetcher next: 1.

	self
		assert: (fetchedElements collect: #contents) asArray 
		equals: #(#Point)
]

{ #category : 'tests' }
CoDependencyPackageScopedGlobalVariableTest >> testWhenNoLocalThenDependentPackages [

	| localFetcher  fetchedElements |
	localFetcher := CoDependencyGlobalVariableFetcher  new.
	localFetcher
		completionClass: ClassWithDependencyOnData2;
		filter: (CoBeginsWithFilter caseSensitive: false filterString: 'An').

	fetchedElements := localFetcher next: 3.

	self assert: fetchedElements size equals: 3.
	self
		assert: (fetchedElements collect: #contents) asArray 
		equals: #(#AnalyserA #AnalyserData1 #AnalyserData1SharedPool)
]
