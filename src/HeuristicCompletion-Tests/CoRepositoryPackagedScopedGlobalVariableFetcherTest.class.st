Class {
	#name : 'CoRepositoryPackagedScopedGlobalVariableFetcherTest',
	#superclass : 'CoMiniTestCase',
	#instVars : [
		'package1',
		'package2',
		'package3',
		'organizer',
		'ai12',
		'ai13',
		'a11',
		'aa1',
		'ai2',
		'ai23',
		'aa2',
		'ai3',
		'a33',
		'blop',
		'aiGlobal1',
		'aiGlobal2'
	],
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'running' }
CoRepositoryPackagedScopedGlobalVariableFetcherTest >> setUp [
	" we have
		2 packages 
			P1A : Ai12 Ai13 A11 AA1 CoInP1
			P2A : Ai2 Ai23 AA2
			P2A : Ai3 A33 Blop
	
	and two globals		
		"
	
	| classesInP1 classesInP2 classesInP3 dict |
	super setUp.
	completionClass := CoMockClass new name: 'CoInP1'. 
	ai12 := CoMockClass new name: 'Ai12'.
	ai13 := CoMockClass new name: 'Ai13'.
	a11 := CoMockClass new name: 'A11'.
	aa1 := CoMockClass new name: 'AA1'.
	
	ai2 := CoMockClass new name: 'Ai2'.
	ai23 := CoMockClass new name: 'Ai23'.
	aa2 := CoMockClass new name: 'AA2'.
	
	ai3 := CoMockClass new name: 'Ai3'.
	a33 := CoMockClass new name: 'A33'.
	blop := CoMockClass new name: 'Blop'.
	
	aiGlobal1 := CoMockClass new name: 'AiGlobal1'.
	aiGlobal2 := CoMockClass new name: 'AiGlobal2'.
		
	package1 := CoMockPackage new. 
	package1 name: 'P1-Core1'.
	classesInP1 := { ai12 . ai13 . a11 . aa1 . completionClass}.
	package1 definedClasses: classesInP1.
	classesInP1 do: [:cl | cl package: package1]. 
	
	package2 := CoMockPackage new. 
	package2 name: 'P1-Core2'.
	classesInP2 :=  { ai2 . ai23 . aa2}.
	package2 definedClasses: classesInP2.
	classesInP2 do: [:cl | cl package: package2]. 
	
	
	package3 := CoMockPackage new. 
	package3 name: 'P1-Test'.
	classesInP3 :=  { ai3 . a33 . blop}.
	package3 definedClasses: classesInP3.
	classesInP3 do: [:cl | cl package: package3]. 
	
	organizer := CoMockPackageOrganizer new.
	organizer packages: { package1 . package2. package3 } asOrderedCollection.
	 { package1 . package2. package3 } do: [ :p | p organizer: organizer ].
	
	
	"We need an environment with keys. We create a dummy dictionary with keys and just the same as value"
	
	dict := Dictionary new. 
	{aiGlobal1 . aiGlobal2 . ai12 .  ai13 . a11 . aa1 . ai2 . ai23 . aa2 . ai3 . a33 . blop}
		do: [ :each | dict at: each name put: each ].
		
	systemNavigation := CoMockSystemNavigation new
		globals: dict;
		yourself.

	fetcher := CoRepositoryGlobalVariableFetcher new. 
	fetcher completionClass: completionClass.
	fetcher systemNavigation: systemNavigation.
	fetcher filter: (CoBeginsWithFilter caseSensitive: false filterString: 'Ai').

	expectedElements := {
		CoGlobalEntry contents: 'Ai12' node: nil .
		CoGlobalEntry contents: 'Ai13' node: nil .
		CoGlobalEntry contents: 'Ai2' node: nil .
		CoGlobalEntry contents: 'Ai23' node: nil .
		CoGlobalEntry contents: 'Ai3' node: nil 
		}
]

{ #category : 'one local one global' }
CoRepositoryPackagedScopedGlobalVariableFetcherTest >> testFirstPackageOfCompletionClass [

	| fetchedElements expected |
	fetchedElements := self fetcher next: 2.
	expected := { self firstExpectedElement . self secondExpectedElement }.
	fetchedElements withIndexDo: [ :each :index |
		self assertEntry: each equals: (expected at: index) ].
	self assert: fetchedElements size equals: expected size
]

{ #category : 'one local one global' }
CoRepositoryPackagedScopedGlobalVariableFetcherTest >> testFirstPackageOfCompletionClassThenAllProjectLevelPackage [

	| fetchedElements expected |

	fetchedElements := self fetcher next: 5.
	expected  := expectedElements first: 5. 
	fetchedElements withIndexDo: [ :each :index |
		self assertEntry: each equals: (expected at: index) ].
	self assert: fetchedElements size equals: expected size
]

{ #category : 'one local one global' }
CoRepositoryPackagedScopedGlobalVariableFetcherTest >> testFirstPackageOfCompletionClassThenAllProjectLevelPackageThenNoGlobals [
	"we do not repo global since this is the next fetcher' job "
	| fetchedElements expected |
	
	fetchedElements := self fetcher next: 7.
	expected  := expectedElements first: 5. 
	fetchedElements withIndexDo: [ :each :index |
		self assertEntry: each equals: (expected at: index) ].
	self assert: fetchedElements size equals: expected size
]

{ #category : 'one local one global' }
CoRepositoryPackagedScopedGlobalVariableFetcherTest >> testFirstPackageOfCompletionClassThenProjectLevelPackage [

	| fetchedElements expected |

	fetchedElements := self fetcher next: 3.
	expected  := expectedElements first: 3. 
	fetchedElements withIndexDo: [ :each :index |
		self assertEntry: each equals: (expected at: index) ].
	self assert: fetchedElements size equals: expected size
]

{ #category : 'one local one global' }
CoRepositoryPackagedScopedGlobalVariableFetcherTest >> testNoClassesMatchingInPackage [
	"Tests the behavior if a package has no classes that match 'Ai'."

	| localPackage localFetcher fetchedElements noMatch |
	
	noMatch := CoMockClass new name: 'NoMatch'.
	localPackage := CoMockPackage new.
	localPackage name: 'Empty-Package'.
	localPackage definedClasses: { noMatch }.
	noMatch package: localPackage.
	localPackage organizer: organizer.
	organizer add: localPackage.
	
	systemNavigation globals at: noMatch name put: noMatch.
	
	localFetcher := CoRepositoryGlobalVariableFetcher new.
	localFetcher
		completionClass: noMatch;
		systemNavigation: systemNavigation;
		filter: (CoBeginsWithFilter caseSensitive: false filterString: 'Ai').

	fetchedElements := localFetcher upToEnd.

	"Because the package is empty and we have no 'completionClass'"

	self assert: fetchedElements size equals: 0.

]

{ #category : 'one local one global' }
CoRepositoryPackagedScopedGlobalVariableFetcherTest >> testNoCompletionClassReturnsZeroEntry [
	"Tests the behavior if a package has no classes that match 'Ai'."

	| localFetcher fetchedElements |
	
	localFetcher := CoRepositoryGlobalVariableFetcher new.
	localFetcher
		completionClass: nil;
		filter: (CoBeginsWithFilter caseSensitive: false filterString: 'Ai').

	fetchedElements := localFetcher upToEnd.

	self assert: fetchedElements size equals: 0.

]

{ #category : 'one local one global' }
CoRepositoryPackagedScopedGlobalVariableFetcherTest >> testRequestMoreThanAvailableOnlyReturnAvailable [

	| fetchedElements expected |

	fetchedElements := self fetcher next: 10.	
	"There is more than 7 classes starting with Ai"
	self assert: fetchedElements size equals: 5.
	
	expected  := expectedElements first: 5. 
	fetchedElements withIndexDo: [ :each :index |
		self assertEntry: each equals: (expected at: index) ].
	self assert: fetchedElements size equals: expected size
]
