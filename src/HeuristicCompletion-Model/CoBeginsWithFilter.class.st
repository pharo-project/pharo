"
I am representing the filters with a begins with.

"
Class {
	#name : 'CoBeginsWithFilter',
	#superclass : 'CoFilter',
	#instVars : [
		'completionString'
	],
	#category : 'HeuristicCompletion-Model-Core',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Core'
}

{ #category : 'instance creation' }
CoBeginsWithFilter class >> caseSensitive: aBoolean filterString: aString [

	aBoolean ifTrue: [
		^ CoCaseSensitiveBeginsWithFilter new
			completionString: aString;
			yourself ]
		ifFalse: [
		^ CoCaseInsensitiveBeginsWithFilter new
			completionString: aString;
			yourself	]
]

{ #category : 'accessing' }
CoBeginsWithFilter >> completionString [

	^ completionString
]

{ #category : 'accessing' }
CoBeginsWithFilter >> completionString: anObject [

	completionString := anObject
]

{ #category : 'testing' }
CoBeginsWithFilter >> fuzzyAcceptsContents: aString [

	^ (self fuzzyScoreForContents: aString) isNotNil
]

{ #category : 'testing' }
CoBeginsWithFilter >> fuzzyScoreForContents: aString [

	^ CoFuzzyMatcher
		scoreForCandidate: aString
		token: completionString
		caseSensitive: self isCaseSensitive
		tolerance: CoCompletionTolerance rate
]

{ #category : 'testing' }
CoBeginsWithFilter >> isCaseSensitive [

	self subclassResponsibility
]

{ #category : 'testing' }
CoBeginsWithFilter >> isLessNarrowThanNegation: anotherFilter [

	^ false
]

{ #category : 'testing' }
CoBeginsWithFilter >> prefixAcceptsContents: aString [

	self subclassResponsibility
]

{ #category : 'testing' }
CoBeginsWithFilter >> requiresFallbackEnumeration [

	^ CoCompletionTolerance enabled
		and: [ completionString size >= CoCompletionTolerance minimumTokenSize ]
]

{ #category : 'testing' }
CoBeginsWithFilter >> sortEntries: aCollection [

	| exact fuzzy |
	self requiresFallbackEnumeration ifFalse: [ ^ aCollection ].

	exact := OrderedCollection new.
	fuzzy := OrderedCollection new.

	aCollection do: [ :each |
		(self prefixAcceptsContents: each contents)
			ifTrue: [ exact add: each ]
			ifFalse: [ fuzzy add: each ] ].

	fuzzy isEmpty ifTrue: [ ^ aCollection ].

	fuzzy sort: [ :a :b |
		| scoreA scoreB |
		scoreA := self fuzzyScoreForContents: a contents.
		scoreB := self fuzzyScoreForContents: b contents.
		scoreA = scoreB
			ifTrue: [ a contents <= b contents ]
			ifFalse: [ scoreA < scoreB ] ].

	^ OrderedCollection new
		addAll: exact;
		addAll: fuzzy;
		yourself
]
