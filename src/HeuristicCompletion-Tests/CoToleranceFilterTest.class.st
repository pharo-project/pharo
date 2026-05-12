Class {
	#name : 'CoToleranceFilterTest',
	#superclass : 'TestCase',
	#category : 'HeuristicCompletion-Tests',
	#package : 'HeuristicCompletion-Tests'
}

{ #category : 'running' }
CoToleranceFilterTest >> setUp [
	super setUp.

	"Put here a common initialization logic for tests"
	CoToleranceFilter enable.
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
