"
Tests for the new algorithm that is diffing changes.
Tests from L. Uzonyi (from squeak trunk System.ul207and 208)
"
Class {
	#name : #TextDiffBuilderTest,
	#superclass : #TestCase,
	#category : #'Tools-Test-DiffBuilder'
}

{ #category : #private }
TextDiffBuilderTest >> convertToString: array [

	^String streamContents: [ :stream |
		array do: [ :each |
			stream nextPutAll: each asString; cr ] ]
]

{ #category : #private }
TextDiffBuilderTest >> patchSequenceFor: x and: y [

	^(TextDiffBuilder
		from: (self convertToString: x)
		to:  (self convertToString: y)) buildPatchSequence
]

{ #category : #tests }
TextDiffBuilderTest >> testEmptyLcs1 [

	| patch |
	patch := self patchSequenceFor: #(a b c) and: #().
	self assert: patch size equals: 3.
	self assert: (patch allSatisfy: [ :each | each key = #remove ])
]

{ #category : #tests }
TextDiffBuilderTest >> testEmptyLcs2 [

	| patch |
	patch := self patchSequenceFor: #() and: #(a b c).
	self assert: patch size equals: 3.	
	self assert: (patch allSatisfy: [ :each | each key = #insert ])
]

{ #category : #tests }
TextDiffBuilderTest >> testEmptyLcs3 [

	| patch |
	patch := self patchSequenceFor: #(a b c) and: #(d e f g).
	self assert: patch size equals: 7.	
	patch do: [ :each |
		each key = #remove ifTrue: [ self assert: ('abc' includes: each value first) ].
		each key = #insert ifTrue: [ self assert: ('defg' includes: each value first) ] ]
]

{ #category : #tests }
TextDiffBuilderTest >> testEmptySequences [
	| patch |
	patch := self patchSequenceFor: #() and: #().
	self assertEmpty: patch
]

{ #category : #tests }
TextDiffBuilderTest >> testIfPatchIsMinimal [
	| patch |
	patch := self patchSequenceFor: #(a a a b) and: #(a b a a).
	self assert: patch size equals: 5.	"lcs is aaa"
	self assert: (patch count: [ :each | each key = #match ]) equals: 3.
	self assert: (patch count: [ :each | each key = #insert ]) equals: 1.
	self assert: (patch count: [ :each | each key = #remove ]) equals: 1.
	patch do: [ :each | each key = #match ifTrue: [ self assert: each value first equals: $a ] ifFalse: [ self assert: each value first equals: $b ] ]
]

{ #category : #tests }
TextDiffBuilderTest >> testIfSequence1 [

	| patch |
	patch := self patchSequenceFor: #(a b c d) and: #(d c b a).
	self assert: patch size equals: 7.	"lcs is any one letter sequence"
	self assert: (patch count: [ :each | each key = #match ]) equals: 1.
	self assert: (patch count: [ :each | each key = #insert ]) equals: 3.
	self assert: (patch count: [ :each | each key = #remove ]) equals: 3.
	patch do: [ :each | 
		each key = #match 
			ifTrue: [ self assert: each value first equals: $d ]
			ifFalse: [ self assert: ('abc' includes: each value first) ] ]
]

{ #category : #tests }
TextDiffBuilderTest >> testIfSequence2 [

	| patch |
	patch := self patchSequenceFor: #(a b c d) and: #(c d b a).
	self assert: patch size equals: 6.	"lcs is cd"
	self assert: (patch count: [ :each | each key = #match ]) equals: 2.
	self assert: (patch count: [ :each | each key = #insert ]) equals: 2.
	self assert: (patch count: [ :each | each key = #remove ]) equals: 2.
	patch do: [ :each | 
		each key = #match 
			ifTrue: [ self assert: ('cd' includes: each value first) ]
			ifFalse: [ self assert: ('ab' includes: each value first) ] ]
]

{ #category : #tests }
TextDiffBuilderTest >> testIfSequence3 [

	| patch |
	patch := self patchSequenceFor: #(a b c d) and: #(b d c a).
	self assert: patch size equals: 6.	"lcs is bd"
	self assert: (patch count: [ :each | each key = #match ]) equals: 2.
	self assert: (patch count: [ :each | each key = #insert ]) equals: 2.
	self assert: (patch count: [ :each | each key = #remove ]) equals: 2.
	patch do: [ :each | 
		each key = #match 
			ifTrue: [ self assert: ('bd' includes: each value first) ]
			ifFalse: [ self assert: ('ac' includes: each value first) ] ]
]

{ #category : #tests }
TextDiffBuilderTest >> testIfSequence4 [

	| patch |
	patch := self patchSequenceFor: #(a b c d) and: #(d b c a).
	self assert: patch size equals: 6.	"lcs is bc"
	self assert: (patch count: [ :each | each key = #match ]) equals: 2.
	self assert: (patch count: [ :each | each key = #insert ]) equals: 2.
	self assert: (patch count: [ :each | each key = #remove ]) equals: 2.
	patch do: [ :each | 
		each key = #match 
			ifTrue: [ self assert: ('bc' includes: each value first) ]
			ifFalse: [ self assert: ('ad' includes: each value first) ] ]
]

{ #category : #tests }
TextDiffBuilderTest >> testIfSequence5 [

	| patch matches nonMatches |
	patch := self patchSequenceFor: #(a b c d) and: #(c d a b).
	self assert: patch size equals: 6.	"lcs is ab or cd"
	matches := (patch select: [ :each | each key = #match ])
		collect: [ :each | each value first ] as: String.
	self assert: (#('ab' 'cd') includes: matches).
	self assert: (patch count: [ :each | each key = #insert ]) equals: 2.
	self assert: (patch count: [ :each | each key = #remove ]) equals: 2.
	nonMatches := #('ab' 'cd') detect: [ :each | each ~= matches ].
	patch do: [ :each | 
		each key = #match 
			ifTrue: [ self assert: (matches includes: each value first) ]
			ifFalse: [ self assert: (nonMatches includes: each value first) ] ]
]

{ #category : #tests }
TextDiffBuilderTest >> testIfSequence6 [

	| patch |
	patch := self patchSequenceFor: #(a b c d) and: #(d a b c).
	self assert: patch size equals: 5.	"lcs is abc"
	self assert: (patch count: [ :each | each key = #match ]) equals: 3.
	self assert: (patch count: [ :each | each key = #insert ]) equals: 1.
	self assert: (patch count: [ :each | each key = #remove ]) equals: 1.
	patch do: [ :each | 
		each key = #match 
			ifTrue: [ self assert: ('abc' includes: each value first) ]
			ifFalse: [ self assert: each value first equals: $d ] ]
]

{ #category : #tests }
TextDiffBuilderTest >> testSameSequence [

	| patch |
	patch := self patchSequenceFor: #(a b c) and: #(a b c).
	self assert: patch size equals: 3.	
	self assert: (patch allSatisfy: [ :each | each key = #match ])
]

{ #category : #tests }
TextDiffBuilderTest >> testSameSequenceWithRepetitions [

	| patch |
	patch := self patchSequenceFor: #(a a b a) and: #(a a b a).
	self assert: patch size equals: 4.	
	self assert: (patch allSatisfy: [ :each | each key = #match ])
]
