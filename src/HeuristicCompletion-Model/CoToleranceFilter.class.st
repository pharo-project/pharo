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

	^ 0.70

	
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

{ #category : 'class initialization' }
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

{ #category : 'class initialization' }
CoToleranceFilter class >> reset [ 

	Rate := self defaultRate.
	MinimumTokenSize := self defaultMinimumTokenSize
	
]

{ #category : 'algorithms' }
CoToleranceFilter class >> scoreForCandidate: aCandidateString token: aTokenString caseSensitive: aBoolean [
	"Returns 1 if the candidate matches the token 
	 Returns 0 if  * the candidate and the token don't start with the same caracter 
						* Token or the candidate is empty
						* The candidate starts exactly with the token
						* The tolerance is not enabled, or the token size does not exceed the minimum size
						* The keyword arity is not stable
	"
	| candidate token distance |
	candidate := aCandidateString asString.
	token := aTokenString asString.

	candidate = token ifTrue: [ ^ 0 ].

	candidate := aBoolean
		ifTrue: [ candidate ]
		ifFalse: [ candidate asLowercase ].
	token := aBoolean
		ifTrue: [ token ]
		ifFalse: [ token asLowercase ].

	(candidate beginsWith: token) ifTrue: [ ^ 0 ].
	(self shouldTryFuzzyFor: token candidate: candidate ) ifFalse: [ ^ 0 ].

	^ distance := AIJaroWinklerDistance new distanceBetween: token and: candidate
]

{ #category : 'algorithms' }
CoToleranceFilter class >> shouldTryFuzzyFor: token candidate: candidate [
	"I return false if : 	* Tolerance is disabled
							 	* The Token or the candidate is empty
								* The token size does not exceeed the minimum token length
								* Thee keyword arity is not stable
								* The first caracter of the candidate and the token does not match
	 Otherwise I return true
	"

	(self enabled not or: [ self rate <= 0.0 ]) ifTrue: [ ^ false ].
	token isEmpty ifTrue: [ ^ false ].
	candidate isEmpty ifTrue: [ ^ false ].
	token size < self minimumTokenSize ifTrue: [ ^ false ].

	"Keep keyword arity stable"
	((token includes: $:) or: [ candidate includes: $: ]) ifTrue: [
		(token occurrencesOf: $:) = (candidate occurrencesOf: $:)
			ifFalse: [ ^ false ] ].

	"Cheap guardrails to avoid noisy matches"
	token first = candidate first ifFalse: [ ^ false ].
	
	^ true
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

	^ (self fuzzyScoreFor: aCandidate) > Rate
]

{ #category : 'testing' }
CoToleranceFilter >> fuzzyScoreFor: aCandidate [

	^ self class scoreForCandidate: aCandidate contents
		token: decoree completionString
		caseSensitive: (decoree class = CoCaseSensitiveBeginsWithFilter)

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
