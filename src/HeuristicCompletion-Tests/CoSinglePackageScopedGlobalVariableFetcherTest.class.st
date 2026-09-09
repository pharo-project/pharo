Class {
	#name : 'CoSinglePackageScopedGlobalVariableFetcherTest',
	#superclass : 'CoMiniTestCase',
	#instVars : [
		'a',
		'b',
		'c',
		'organizer'
	],
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'running' }
CoSinglePackageScopedGlobalVariableFetcherTest >> setUp [
	
	super setUp.
	completionClass := CoMockClass new name: 'CoInP1'. 
	a := CoMockClass new name: 'AInP1'.
	b := CoMockClass new name: 'AAInP1'.
	c := CoMockClass new name: 'AA'.
	
	package := CoMockPackage new. 
	package definedClasses: { a . b . c . completionClass }.
	 { a . b . c . completionClass } do: [ :each | each package: package ].
	
	organizer := CoMockPackageOrganizer new.
	organizer packages: { package } asOrderedCollection.
	{ package } do: [ :p | p organizer: organizer ].

]

{ #category : 'running' }
CoSinglePackageScopedGlobalVariableFetcherTest >> testFirstPackageLevel [

	| fetchedElements expected |
	
	fetcher := CoSinglePackageScopedGlobalVariableFetcher new. 
	fetcher completionClass: a.
	fetcher systemNavigation: systemNavigation.
	fetcher filter: (CoBeginsWithFilter caseSensitive: false filterString: 'AI').
	
	expectedElements := {
		CoGlobalEntry contents: 'AInP1' node: nil }.
	
	fetchedElements := self fetcher next: 1.
	expected := { self firstExpectedElement }.
	fetchedElements withIndexDo: [ :each :index |
		self assertEntry: each equals: (expected at: index) ].
	self assert: fetchedElements size equals: expected size
]

{ #category : 'running' }
CoSinglePackageScopedGlobalVariableFetcherTest >> testResetShouldRestartGenerator [

	| fetchedElements |
	
	fetcher := CoSinglePackageScopedGlobalVariableFetcher new. 
	fetcher completionClass: a.
	fetcher systemNavigation: systemNavigation.
	fetcher filter: (CoBeginsWithFilter caseSensitive: false filterString: 'AI').
	
	expectedElements := {
		CoGlobalEntry contents: 'AInP1' node: nil }.
	
	self fetcher upToEnd; reset.
	fetchedElements := self fetcher next: expectedElements size + 1.
	fetchedElements withIndexDo: [ :each :index |
		self assertEntry: each equals: (expectedElements at: index) ].
	self assert: fetchedElements size equals: expectedElements size
]
