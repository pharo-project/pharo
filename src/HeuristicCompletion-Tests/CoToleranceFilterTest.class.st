Class {
	#name : 'CoToleranceFilterTest',
	#superclass : 'TestCase',
	#instVars : [
		'oldRate',
		'oldMinimumTokenize',
		'filter',
		'entries'
	],
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'running' }
CoToleranceFilterTest >> setUp [
	super setUp.

	"Put here a common initialization logic for tests"
	oldRate := CoToleranceFilter rate. 
	oldMinimumTokenize := CoToleranceFilter minimumTokenSize. 
	CoToleranceFilter enable.
	
	filter := CoToleranceFilter new.
	
	entries := OrderedCollection new.
]

{ #category : 'running' }
CoToleranceFilterTest >> tearDown [ 
	
	CoToleranceFilter rate: oldRate. 
	CoToleranceFilter minimumTokenSize: oldMinimumTokenize. 
	
	super tearDown 
]

{ #category : 'tests' }
CoToleranceFilterTest >> testComplementResultExactAppearBeforeFuzzy [
	| result |
	entries add: 'foo'; add: 'for'; add: 'fuzzy'. 
	filter decoree: (CoCaseSensitiveBeginsWithFilter new completionString: 'fo'). 
	result := (filter complementResults: entries). 
	self assert: result first equals: 'foo'.
	self assert: result second equals: 'for'.
	self assert: result third equals: 'fuzzy'.
]

{ #category : 'tests' }
CoToleranceFilterTest >> testComplementResultSortFuzzy [
	| result |
	entries add: 'fuzzy'; add: 'fuzz'. 
	filter decoree: (CoCaseSensitiveBeginsWithFilter new completionString: 'fo'). 
	result := (filter complementResults: entries). 
	"We should get fuzz before fuzzy since fuzz is closer to the token"
	self assert: result first equals: 'fuzz'.
	self assert: result second equals: 'fuzzy'.

]

{ #category : 'tests' }
CoToleranceFilterTest >> testComplementResultWhenNoFallbackEnumerationBecauseDisabled [
	CoToleranceFilter disable. 
	
	entries add: 'foo'; add: 'bar'.
	
	self assert: (filter complementResults: entries) equals: (entries).
]

{ #category : 'tests' }
CoToleranceFilterTest >> testComplementResultWhenNoFallbackEnumerationBecauseSmallTokenSize [
	filter decoree: (CoCaseSensitiveBeginsWithFilter new completionString: 'f').
	
	entries add: 'foo'; add: 'bar'.
	
	self assert: (filter complementResults: entries) equals: (entries).
]

{ #category : 'tests' }
CoToleranceFilterTest >> testRequiresFallbackEnumerationWhenTokenSizeIsLessThanMinimumTokenSize [
	filter decoree: CoCaseInsensitiveBeginsWithFilter new.
	filter decoree completionString: 'a'.
	self deny: (filter requiresFallbackEnumeration ) .
	
]

{ #category : 'tests' }
CoToleranceFilterTest >> testRequiresFallbackEnumerationWhenToleranceDisabled [
	CoToleranceFilter disable. 
	self deny: (filter requiresFallbackEnumeration )
]

{ #category : 'tests' }
CoToleranceFilterTest >> testScoreForCandidatesWWhenTransposition [ 
	"distance = 1/5, max edits = 0.4 * 5 = 2, with one transposition "
	self assert: (CoToleranceFilter scoreForCandidate: 'hlelo' token:'hello'  caseSensitive: false tolerance: 0.2) equals: 0.2

]

{ #category : 'tests' }
CoToleranceFilterTest >> testScoreForCandidatesWhenExactMatch [ 
	self assert: (CoToleranceFilter scoreForCandidate: 'foo' token:'foo'  caseSensitive: false tolerance: 0.3) equals: 0
]

{ #category : 'tests' }
CoToleranceFilterTest >> testScoreForCandidatesWhenExactMatchWhenCaseSensitive [ 
	self assert: (CoToleranceFilter scoreForCandidate: 'FoO' token:'FoO'  caseSensitive: true tolerance: 0.3) equals: 0
]

{ #category : 'tests' }
CoToleranceFilterTest >> testScoreForCandidatesWhenOneInsertion [
	self assert: (CoToleranceFilter scoreForCandidate: 'helo' token:'hello'  caseSensitive: false tolerance: 0.4) equals: 0.2
]

{ #category : 'tests' }
CoToleranceFilterTest >> testScoreForCandidatesWhenOneSubstitution [ 
	self assert: (CoToleranceFilter scoreForCandidate: 'hello' token:'hallo'  caseSensitive: false tolerance: 0.4) equals: 0.2
]

{ #category : 'tests' }
CoToleranceFilterTest >> testScoreForCandidatesWhenOneSubstitutionWithLowToleranceReturnZero [
	
	self assert: (CoToleranceFilter scoreForCandidate: 'hello' token:'hallo'  caseSensitive: false tolerance: 0) equals: 0
]

{ #category : 'tests' }
CoToleranceFilterTest >> testScoreForCandidatesWhenTranspositionWithFirstCaracterDoesNotMustShouldReturnZero [ 
	"distance = 1/5, max edits = 0.4 * 5 = 2, with one transposition "
	self assert: (CoToleranceFilter scoreForCandidate: 'Hello' token:'hello'  caseSensitive: true tolerance: 0.4) equals: 0
]

{ #category : 'tests' }
CoToleranceFilterTest >> testShouldTryFuzzyForWhenEmptyCandidate [ 
	self assert: (CoToleranceFilter shouldTryFuzzyFor: 'foo' candidate: '' tolerance: 0.4) equals: false
]

{ #category : 'tests' }
CoToleranceFilterTest >> testShouldTryFuzzyForWhenEmptyToken [ 
	self assert: (CoToleranceFilter shouldTryFuzzyFor: '' candidate: 'foo' tolerance: 0.4) equals: false
]

{ #category : 'tests' }
CoToleranceFilterTest >> testShouldTryFuzzyForWhenFirstCaracterMismatch [ 
	self assert: (CoToleranceFilter shouldTryFuzzyFor: 'At:put:ifAbsent:' candidate: 'at:put:IfAbsent:' tolerance: 0.4) equals: false

]

{ #category : 'tests' }
CoToleranceFilterTest >> testShouldTryFuzzyForWhenKeywordStructureIsDifferent [ 
	
	self assert: (CoToleranceFilter shouldTryFuzzyFor: 'at:put:ifAbsent:' candidate: 'at:putIfAbsent:' tolerance: 0.4) equals: false

]

{ #category : 'tests' }
CoToleranceFilterTest >> testShouldTryFuzzyForWhenKeywordStructureIsSimilar [ 

	self assert: (CoToleranceFilter shouldTryFuzzyFor: 'at:put:ifAbsent:' candidate: 'at:put:IfAbsent:' tolerance: 0.4) equals: true


]

{ #category : 'tests' }
CoToleranceFilterTest >> testShouldTryFuzzyForWhenLengthExceedsToleranceBound [ 
	"maxEdits = 0.4 * 32 ≈ 13.
	Length difference = 32 - 5 = 27.
	Since 27 > maxEdits, fuzzy matching should be rejected."
	self assert: (CoToleranceFilter shouldTryFuzzyFor: 'CoAST' candidate: 'CoASTHeuristicsResultSetBuilder' tolerance: 0.4) equals: false

]

{ #category : 'tests' }
CoToleranceFilterTest >> testShouldTryFuzzyForWhenTokenTooShort [ 
	"The minimum token size is 2"
	self assert: (CoToleranceFilter shouldTryFuzzyFor: 'f' candidate: 'foo' tolerance: 0.4) equals: false
]
