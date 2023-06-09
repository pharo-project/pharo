"
Tests for FuzzyMatcher. 
"
Class {
	#name : #FuzzyMatcherTest,
	#superclass : #TestCase,
	#category : #'FuzzyMatcher-Tests-Tests'
}

{ #category : #asserting }
FuzzyMatcherTest >> assert: matcher scoreFor: aString equals: aBlockOrNumber [

	matcher match: aString ifScored: [ :score |
		self assert: score equals: aBlockOrNumber value.
		^ self	 ].

	self error: 'Expected to match: ', aString
]

{ #category : #running }
FuzzyMatcherTest >> newMatcher [

	^ FuzzyMatcher new
]

{ #category : #tests }
FuzzyMatcherTest >> testAllMatchingIn [

	| input output |

	input := #('0g' 'ogb' 'gat' 'dog' 'FroG' 'hog' 'goh' 'hgo').
	output := FuzzyMatcher allMatching: 'og' in: input.

	self assert: output asArray equals: #('ogb' 'dog' 'FroG' 'hog')
]

{ #category : #tests }
FuzzyMatcherTest >> testAssumptions [

	self
		assert: (((1 - 1) * 3) max: -9) equals: 0;
	 	assert: ((2 - 1 - 1) negated) equals: 0;
		assert: ('a' findString: 'a' startingAt: 2 caseSensitive: false) equals: 0
]

{ #category : #tests }
FuzzyMatcherTest >> testChangePattern [

	| matcher |
	matcher := self newMatcher.

	self
		assert: matcher pattern equals: '';
		assert: (matcher matches: 'abcdefghijkl');
		assert: (matcher matches: '').

	matcher pattern: 'afg'.

	self
		assert: (matcher matches: 'abcdefghijkl');
		deny: (matcher matches: '')
]

{ #category : #tests }
FuzzyMatcherTest >> testClassSideAPI [

	"If the protocol is changed, so should the class comment."
	FuzzyMatcher class protocolNames detect: [ :p | p = #'utilities - api' ].

	self
		assert: (FuzzyMatcher allMatching: #a in: #(a b ab))
		equals: #(a ab);

		assert: (FuzzyMatcher allMatching: #a in: { #a -> 1 . #b -> 2 . #acb -> 3 } by: [:each | each key ])
		equals: { #a -> 1 . #acb -> 3 };

		assert: (FuzzyMatcher allSortedByScoreMatching: #b in: { #aaca .#aAaaB . #Aaab . #baaa . #aaba })
		equals: #(baaa aaba Aaab aAaaB)
]

{ #category : #tests }
FuzzyMatcherTest >> testEmptyPattern [

	| matcher |
	matcher := self newMatcher.

	self
		assertEmpty: matcher pattern;
		assert: matcher scoreFor: '' equals: 0;
		assert: matcher scoreFor: 'abc' equals: 0;
		assert: matcher scoreFor: '   ' equals: 0
]

{ #category : #tests }
FuzzyMatcherTest >> testIsSeparator [

	| matcher |
	matcher := self newMatcher.

	self
		assert: (matcher isSeparator: $_);
		assert: (matcher isSeparator: $:);
		deny: (matcher isSeparator: $!);
		deny: (matcher isSeparator: $a)
]

{ #category : #tests }
FuzzyMatcherTest >> testMatching [

	| matcher |
	matcher := self newMatcher.
	matcher pattern: 'a'.

	self
		assert: (matcher matches: 'a');
		assert: (matcher matches: String cr, String tab, String lf, String space, 'a');
		assert: (matcher matches: ',;:_-!"#¤%&/?+\()a')
]

{ #category : #tests }
FuzzyMatcherTest >> testScoreValues [

	| matcher |

	matcher := self newMatcher.

	matcher pattern: 'a'.

	self

		assert: matcher
		scoreFor: 'a'
		equals: (matcher firstLetterBonus + matcher caseEqualBonus);

		assert: matcher
		scoreFor: 'A'
		equals: (matcher firstLetterBonus);

		assert: matcher
		scoreFor: 'ab'
		equals: (matcher firstLetterBonus + matcher caseEqualBonus + matcher unmatchedLetterPenalty);

		assert: matcher
		scoreFor: '1a'
		equals: (matcher leadingLetterPenalty + matcher unmatchedLetterPenalty + matcher caseEqualBonus);

		assert: matcher
		scoreFor: '12345a'
		equals: (matcher maxLeadingLetterPenalty + (5 * matcher unmatchedLetterPenalty ) + (matcher caseEqualBonus)).

	matcher pattern: 'ab'.

	self

		assert: matcher
		scoreFor: 'ab'
		equals: [ "delayed bonus calculation since #indexscore is based on state after matching"
			matcher firstLetterBonus
			+  matcher adjacencyBonus
			+ (matcher caseEqualBonus * 2)
			+ (matcher adjacentCaseEqualBonus)
			+  matcher indexScore
		];

		assert: matcher
		scoreFor: 'A:B'
		equals: (
			matcher firstLetterBonus
			+ matcher separatorBonus
			+ matcher unmatchedLetterPenalty
		);

		assert: matcher
		scoreFor: 'able'
		equals: [
			matcher firstLetterBonus
			+  matcher adjacencyBonus
			+  matcher adjacentCaseEqualBonus
			+ (matcher caseEqualBonus * 2)
			+ (matcher unmatchedLetterPenalty * 2)
			+  matcher indexScore
		]
]

{ #category : #tests }
FuzzyMatcherTest >> testSortedByScore [

	| input output |

	input := #('abc' 'a'  'whj' 'cab' 'cat' 'dog' 'ab').

	output := FuzzyMatcher allSortedByScoreMatching: 'a' in: input.

	self assertCollection: output asArray equals: #('a' 'ab' 'abc' 'cab' 'cat')
]
