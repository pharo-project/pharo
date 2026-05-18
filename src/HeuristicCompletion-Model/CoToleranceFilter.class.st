"
I let the user enter typos. I have a rate of acceptance. I use an edit distance to propose alternative.
"
Class {
	#name : 'CoToleranceFilter',
	#superclass : 'CoFilter',
	#instVars : [
		'decoree'
	],
	#classVars : [
		'MinimumTokenSize',
		'Rate'
	],
	#category : 'HeuristicCompletion-Model-Core',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Core'
}

{ #category : 'default values' }
CoToleranceFilter class >> defaultMinimumTokenSize [

	^ 2

	
]

{ #category : 'default values' }
CoToleranceFilter class >> defaultRate [

	^ 0.75

	
]

{ #category : 'configuration' }
CoToleranceFilter class >> disable [

	Rate := -1
]

{ #category : 'configuration' }
CoToleranceFilter class >> enable [

	Rate := self defaultRate 
]

{ #category : 'configuration' }
CoToleranceFilter class >> enabled [

	^ Rate > 0.0
]

{ #category : 'class initialize' }
CoToleranceFilter class >> initialize [ 

	self reset
]

{ #category : 'configuration' }
CoToleranceFilter class >> minimumTokenSize [

	^ MinimumTokenSize
]

{ #category : 'configuration' }
CoToleranceFilter class >> minimumTokenSize: anInteger [ 

	MinimumTokenSize := (anInteger ifNil: [ self defaultMinimumTokenSize ]) max: 1
]

{ #category : 'configuration' }
CoToleranceFilter class >> rate [

	^ Rate
]

{ #category : 'configuration' }
CoToleranceFilter class >> rate: aNumber [ 

	| value |
	value := (aNumber ifNil: [ self defaultRate ]) asFloat.
	Rate := (value max: 0.0) min: 1.0
]

{ #category : 'class initialize' }
CoToleranceFilter class >> reset [ 

	Rate := self defaultRate.
	MinimumTokenSize := self defaultMinimumTokenSize
	
]

{ #category : 'algorithms' }
CoToleranceFilter class >> scoreForCandidate: aCandidateString token: aTokenString caseSensitive: aBoolean tolerance: aTolerance [
	"Returns 1 if the candidate matches the tokenm 0 otherwise"
	| candidate token maxSize maxEdits distance |
	candidate := aCandidateString asString.
	token := aTokenString asString.

	token isEmpty ifTrue: [ ^ 0 ].
	candidate = token ifTrue: [ ^ 0 ].

	candidate := aBoolean
		ifTrue: [ candidate ]
		ifFalse: [ candidate asLowercase ]
		.
	token := aBoolean
		ifTrue: [ token ]
		ifFalse: [ token asLowercase ].

	(candidate beginsWith: token) ifTrue: [ ^ 0 ].
	(self shouldTryFuzzyFor: token candidate: candidate tolerance: aTolerance) ifFalse: [ ^ 0 ].

	maxSize := token size max: candidate size.
	maxEdits := (aTolerance * maxSize) ceiling.
	maxEdits <= 0 ifTrue: [ ^ 0 ].

	^ distance := AIJaroWinklerDistance new distanceBetween: token and: candidate.
]

{ #category : 'algorithms' }
CoToleranceFilter class >> shouldTryFuzzyFor: token candidate: candidate tolerance: aTolerance [ 

	| maxSize maxEdits |
	(self enabled not or: [ aTolerance <= 0.0 ]) ifTrue: [ ^ false ].
	token isEmpty ifTrue: [ ^ false ].
	candidate isEmpty ifTrue: [ ^ false ].
	token size < self minimumTokenSize ifTrue: [ ^ false ].

	"Keep keyword arity stable"
	((token includes: $:) or: [ candidate includes: $: ]) ifTrue: [
		(token occurrencesOf: $:) = (candidate occurrencesOf: $:)
			ifFalse: [ ^ false ] ].

	"Cheap guardrails to avoid noisy matches"
	token first = candidate first ifFalse: [ ^ false ].
	maxSize := token size max: candidate size.
	maxEdits := (aTolerance * maxSize) ceiling.
	^ (token size - candidate size) abs <= maxEdits
]

{ #category : 'testing' }
CoToleranceFilter >> accepts: aCandidate [

	^ (decoree accepts: aCandidate) or: [ self class enabled and: [ self fuzzyAccepts: aCandidate ]]
]

{ #category : 'testing' }
CoToleranceFilter >> complementResults: aCollection [ 

	| exact fuzzy |
	self requiresFallbackEnumeration ifFalse: [ ^ aCollection ].

	exact := OrderedCollection new.
	fuzzy := OrderedCollection new.

	aCollection do: [ :each |
		(decoree accepts: each)
			ifTrue: [ exact add: each ]
			ifFalse: [ (self fuzzyAccepts: each) ifTrue: [ fuzzy add: each ] ] ].

	fuzzy isEmpty ifTrue: [ ^ aCollection ].

	fuzzy sort: [ :a :b |
		| scoreA scoreB |
		scoreA := self fuzzyScoreFor: a.
		scoreB := self fuzzyScoreFor: b.
		scoreA = scoreB
			ifTrue: [ a contents <= b contents ]
			ifFalse: [ scoreA > scoreB ] ].

	^ OrderedCollection new
		addAll: exact;
		addAll: fuzzy;
		yourself

]

{ #category : 'accessing' }
CoToleranceFilter >> completionString [

	^ decoree completionString
]

{ #category : 'testing' }
CoToleranceFilter >> decoree [

	^ decoree 
]

{ #category : 'testing' }
CoToleranceFilter >> decoree: aFilter [

	decoree := aFilter 
]

{ #category : 'testing' }
CoToleranceFilter >> fuzzyAccepts: aCandidate [

	^ (self fuzzyScoreFor: aCandidate) isZero not
]

{ #category : 'testing' }
CoToleranceFilter >> fuzzyScoreFor: aCandidate [

	^ self class scoreForCandidate: aCandidate contents
		token: decoree completionString
		caseSensitive: (decoree class = CoCaseSensitiveBeginsWithFilter)
		tolerance: Rate

]

{ #category : 'testing' }
CoToleranceFilter >> isCaseSensitive [

	^ decoree isCaseSensitive
]

{ #category : 'testing' }
CoToleranceFilter >> isLessNarrowThanCaseInsensitive: anotherFilter [

	^ decoree isLessNarrowThanCaseInsensitive: anotherFilter
]

{ #category : 'testing' }
CoToleranceFilter >> isLessNarrowThanCaseSensitive: anotherFilter [

	^ decoree isLessNarrowThanCaseSensitive: anotherFilter
]

{ #category : 'testing' }
CoToleranceFilter >> isLessNarrowThanNegation: anotherFilter [

	^ decoree isLessNarrowThanNegation: anotherFilter
]

{ #category : 'testing' }
CoToleranceFilter >> isMoreNarrowThan: anotherFilter [

	^ decoree isMoreNarrowThan: anotherFilter
]

{ #category : 'testing' }
CoToleranceFilter >> requiresFallbackEnumeration [ 

	^ self class  enabled
		and: [ decoree completionString size >= self class minimumTokenSize ]
]
