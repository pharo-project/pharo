Class {
	#name : 'CoGlobalVariableFetcherAdditionalTest',
	#superclass : 'CoBasicFetcherWithElementsTest',
	#instVars : [
		'rate'
	],
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'running' }
CoGlobalVariableFetcherAdditionalTest >> setUp [

	super setUp.
	rate := CoToleranceFilter rate.
	CoToleranceFilter disable.
	systemNavigation := CoMockSystemNavigation new
		globals: {'Ab' . 'B' . 'AIGlobal' . 'AIa' };
		yourself.

	fetcher := CoGlobalVariableFetcher new.
	
	fetcher systemNavigation: systemNavigation.
	fetcher filter: (CoBeginsWithFilter caseSensitive: false filterString: 'AI').

	completionClass := CoMockClass new name: 'CoInP1'. 
	
	expectedElements := {
		CoGlobalEntry contents: 'AIGlobal' node: nil .
		CoGlobalEntry contents: 'AIa' node: nil.
		  }
]

{ #category : 'running' }
CoGlobalVariableFetcherAdditionalTest >> tearDown [ 
	CoToleranceFilter rate: rate.
	super tearDown 
]

{ #category : 'running' }
CoGlobalVariableFetcherAdditionalTest >> testCaseSensitive [ 

	"Tests that a case-sensitive filter 'AI' does not return e.g. 'aiGlobal'"
	
	| localFetcher fetchedElements expected localSystemNavigation |
	localSystemNavigation := CoMockSystemNavigation new
		globals: {'AIGlobal' . 'AIa' . 'aiGlobal' . 'aiSomething' };
		yourself.

	localFetcher := CoGlobalVariableFetcher new.
	localFetcher 
		systemNavigation: localSystemNavigation;
		filter: (CoBeginsWithFilter caseSensitive: true filterString: 'AI').

	fetchedElements := localFetcher upToEnd.

	"This should only return 'AIGlobal' and 'AIa' ignoring 'aiGlobal' or 'aiSomething'"
	expected := {
		CoGlobalEntry contents: 'AIGlobal' node: nil .
		CoGlobalEntry contents: 'AIa' node: nil
	}.

	self assert: fetchedElements size equals: 2.
	fetchedElements withIndexDo: [ :each :index |
		self assertEntry: each equals: (expected at: index) ].
]

{ #category : 'running' }
CoGlobalVariableFetcherAdditionalTest >> testEmptyPrefixShouldReturnEverything [
	"Tests fetching with an empty prefix, which should return everything in the environment."
	
	| localFetcher fetchedElements localSystemNavigation globals |
	globals := {'Ab' . 'B' . 'AIGlobal' . 'AIa' }.
	localSystemNavigation := CoMockSystemNavigation new
		globals: globals;
		yourself.

	localFetcher := CoGlobalVariableFetcher new.
	localFetcher 
		systemNavigation: localSystemNavigation;
		filter: (CoBeginsWithFilter caseSensitive: false filterString: '').

	fetchedElements := localFetcher upToEnd.

	self 
		assert: fetchedElements size 
		equals: globals size.
	
	"Check contents match. The order is determined by the fetcher logic, 
	 so we only check the set equivalence for simplicity here."

	self 
		assert: (fetchedElements collect: #contents) asSet 
		equals: globals asSet
]

{ #category : 'running' }
CoGlobalVariableFetcherAdditionalTest >> testEnvironment [ 

	| o | 
	o := OrderedCollection new. 
	systemNavigation 
			allGlobalNamesStartingWith: 'AI' 
			do: [ :each | o add: each ] caseSensitive: false.
	self assert: o first equals: 'AIGlobal'.
	self assert: o second equals: 'AIa'.
	self assert: o size equals: 2.

]

{ #category : 'running' }
CoGlobalVariableFetcherAdditionalTest >> testFirst [

	| fetchedElements expected |
	fetchedElements := self fetcher next: 1.
	expected := { self firstExpectedElement }.
	fetchedElements withIndexDo: [ :each :index |
		self assertEntry: each equals: (expected at: index) ].
	self assert: fetchedElements size equals: expected size
]

{ #category : 'running' }
CoGlobalVariableFetcherAdditionalTest >> testFirstThreeButOnlyTwo [

	| fetchedElements  |
	fetchedElements := self fetcher next: 3.
	fetchedElements withIndexDo: [ :each :index |
		self assertEntry: each equals: (expectedElements at: index) ].
	self assert: fetchedElements size equals: expectedElements size
]

{ #category : 'running' }
CoGlobalVariableFetcherAdditionalTest >> testNoGlobalsInEnvironment [ 

	"Tests behavior when the environment is empty"
	
	| localFetcher fetchedElements localSystemNavigation |
	localSystemNavigation := CoMockSystemNavigation new
		globals: #(); 
		yourself.

	localFetcher := CoGlobalVariableFetcher new.
	localFetcher 
		systemNavigation: localSystemNavigation;
		filter: (CoBeginsWithFilter caseSensitive: false filterString: 'AI').

	fetchedElements := localFetcher upToEnd.
	self assert: fetchedElements isEmpty
]

{ #category : 'running' }
CoGlobalVariableFetcherAdditionalTest >> testNoMatches [
	"Tests when there is no global matching the prefix."
	
	| localFetcher fetchedElements localSystemNavigation |
	localSystemNavigation := CoMockSystemNavigation new
		globals: {'AIGlobal' . 'AIa' . 'GlobalSomething' };
		yourself.

	localFetcher := CoGlobalVariableFetcher new.
	localFetcher 
		systemNavigation: localSystemNavigation;
		filter: (CoBeginsWithFilter caseSensitive: false filterString: 'XYZ').

	fetchedElements := localFetcher upToEnd.
	self assert: fetchedElements isEmpty.
]
