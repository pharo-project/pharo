"
FuzzyMatcher is an approximate string matching algorithm that can determine if a string includes a given pattern.
For example, the string 'axby' matches both the pattern 'ab' and, 'ay', but not 'ba'. 

The algorithm is based on lib_fts[1], and includes an optional scoring algorithm that can be used to sort all the matches based on their similarity to the pattern.

1: 
https://blog.forrestthewoods.com/reverse-engineering-sublime-text-s-fuzzy-match-4cffeed33fdb
https://github.com/forrestthewoods/lib_fts

"
Class {
	#name : #FuzzyMatcher,
	#superclass : #Object,
	#instVars : [
		'pattern',
		'lowercasePattern',
		'indexes'
	],
	#category : #'FuzzyMatcher-Base'
}

{ #category : #'utilities - api' }
FuzzyMatcher class >> allMatching: aPattern in: aCollectionOfStrings [

	| matcher |
	
	matcher := self pattern: aPattern.

	^ aCollectionOfStrings select: [:each | matcher matches: each ]
]

{ #category : #'utilities - api' }
FuzzyMatcher class >> allMatching: aPattern in: aCollectionOfStrings by: aBlockReturningString [

	| matcher |
	
	matcher := self pattern: aPattern.

	^ aCollectionOfStrings select: [:each | matcher matches: (aBlockReturningString value: each) ]
]

{ #category : #'utilities - api' }
FuzzyMatcher class >> allSortedByScoreMatching: aPattern in: aCollectionOfStrings [
	
	^ self allSortedByScoreMatching: aPattern in: aCollectionOfStrings by: [:each | each ]
]

{ #category : #'utilities - api' }
FuzzyMatcher class >> allSortedByScoreMatching: aPattern in: aCollectionOfStrings by: aBlockReturningString [

	| matcher matches |
	
	aPattern isEmpty ifTrue: [ ^aCollectionOfStrings asArray ].
	
	matcher := self pattern: aPattern.
	matches := OrderedCollection new: aCollectionOfStrings size // 2.
	
	aCollectionOfStrings do: [:each | 
		matcher 
			match: (aBlockReturningString value: each) 
			ifScored: [:score | matches add: score -> each ] ].
	
	matches sort: [:a :b | a key >= b key ].
	
	^ matches collect: [:each | each value ] as: Array
	
]

{ #category : #construction }
FuzzyMatcher class >> pattern: aString [

	^self new
		pattern: aString;
		yourself
]

{ #category : #'scoring - bonus' }
FuzzyMatcher >> adjacencyBonus [

	^ 5
]

{ #category : #'scoring - bonus' }
FuzzyMatcher >> adjacencyIncrease [

	^ 1.2
]

{ #category : #'scoring - bonus' }
FuzzyMatcher >> adjacentCaseEqualBonus [

	^ 3
]

{ #category : #'scoring - bonus' }
FuzzyMatcher >> caseEqualBonus [

	^ 7
]

{ #category : #'scoring - bonus' }
FuzzyMatcher >> firstLetterBonus [

	^ 12
]

{ #category : #'private - accessing' }
FuzzyMatcher >> firstScore: aString at: anIndex [

	| score |

	score := (aString at: anIndex) = pattern first 
		ifTrue: [ self caseEqualBonus ]
		ifFalse: [ 0 ].
	
	anIndex = 1 	ifTrue: [ ^ score + self firstLetterBonus ].
		
	score := score + (((anIndex - 1) * self leadingLetterPenalty) max: self maxLeadingLetterPenalty).
				
	^ score 
]

{ #category : #'private - accessing' }
FuzzyMatcher >> indexScore [ 

	| sum ramp |
	
	ramp := 1.
	sum := 0.
	
	1 to: indexes size - 1 do: [ :ix |
		ramp := (indexes at: ix) + 1 = (indexes at: ix + 1) 
			ifTrue: [ ramp + (ramp * self adjacencyIncrease) ]
			ifFalse: [ 1 ].		
		
		sum := sum + ramp - 1
	].
	
	^ sum rounded
]

{ #category : #initialization }
FuzzyMatcher >> initialize [

	super initialize.
	pattern := lowercasePattern := ''.
	indexes := #()	
]

{ #category : #'private - testing' }
FuzzyMatcher >> isSeparator: aCharacter [
	
	^  #'_:' includes: aCharacter
]

{ #category : #'scoring - penalty' }
FuzzyMatcher >> leadingLetterPenalty [

	^ -3
]

{ #category : #comparing }
FuzzyMatcher >> match: aString ifScored: aBlock [
	
	| score |
	
	score := 0.	
		
	pattern ifEmpty: [ aBlock value: score. ^ self ].
	
	(self matches: aString) ifFalse: [ ^ self ].
	
	score := self firstScore: aString at: indexes first.
	
	2 to: pattern size do: [ :pix | 
		score := score + (self score: aString at: (indexes at: pix) patternAt: pix) ].
		
	score := score + self indexScore + ((aString size - pattern size) * self unmatchedLetterPenalty).
		
	aBlock value: score
]

{ #category : #comparing }
FuzzyMatcher >> matches: aString [

	| idx |
	
	pattern size > aString size ifTrue: [ ^ false ].

	idx := 0.
	pattern withIndexDo: [ :each :i |
		idx := aString 
			findString: each asString 
			startingAt: idx + 1 
			caseSensitive: false. 
		
		idx == 0 ifTrue: [ ^ false ].
		indexes at: i put: idx.
	].

	^ true
]

{ #category : #'scoring - penalty' }
FuzzyMatcher >> maxLeadingLetterPenalty [

	^ -9
]

{ #category : #accessing }
FuzzyMatcher >> pattern [

	^ pattern 
]

{ #category : #accessing }
FuzzyMatcher >> pattern: aString [

	pattern := aString.
	lowercasePattern := pattern asLowercase.
	indexes := Array new: pattern size
	

]

{ #category : #'private - accessing' }
FuzzyMatcher >> score: aString at: stringIndex patternAt: patternIndex [

	| score prev |
	
	prev := (aString at: stringIndex - 1).
	
	score := (self isSeparator: prev) 
		ifTrue: [ self separatorBonus ]
		ifFalse: [ (prev asLowercase = (lowercasePattern at: patternIndex - 1))
			ifTrue: [ 
				self adjacencyBonus + 
				((prev = (pattern at: patternIndex - 1)) ifTrue: [ self adjacentCaseEqualBonus ] ifFalse: [ 0 ]) 
			] 
			ifFalse: [ 0 ] 
		].
	
	(aString at: stringIndex) = (pattern at: patternIndex) ifTrue: [ 
		score := score + self caseEqualBonus.
	].

	^ score 
]

{ #category : #'scoring - bonus' }
FuzzyMatcher >> separatorBonus [

	^ 5
]

{ #category : #'scoring - penalty' }
FuzzyMatcher >> unmatchedLetterPenalty [

	^ -1
]
