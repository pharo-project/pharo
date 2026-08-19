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

{ #category : 'algorithms' }
CoToleranceFilter class >> damerauLevenshteinDistanceBetween: source and: target maxDistance: maxDistance [

	| previousRow currentRow twoRowsBack minInRow cost insertion deletion substitution value |
	source = target ifTrue: [ ^ 0 ].

	previousRow := Array new: target size + 1.
	1 to: (target size + 1) do: [ :j |
		previousRow at: j put: j - 1 ].

	1 to: source size do: [ :i |
		currentRow := Array new: target size + 1.
		currentRow at: 1 put: i.
		minInRow := i.

		1 to: target size do: [ :j |
			cost := ((source at: i) = (target at: j))
				ifTrue: [ 0 ]
				ifFalse: [ 1 ].

			insertion := (currentRow at: j) + 1.
			deletion := (previousRow at: (j + 1)) + 1.
			substitution := (previousRow at: j) + cost.

			value := insertion min: deletion.
			value := value min: substitution.

			(i > 1 and: [ j > 1 and: [
				(source at: i) = (target at: (j - 1))
					and: [ (source at: (i - 1)) = (target at: j) ] ] ]) ifTrue: [
						twoRowsBack ifNotNil: [
							value := value min: ((twoRowsBack at: (j - 1)) + 1) ] ].

			currentRow at: (j + 1) put: value.
			minInRow := minInRow min: value ].

		minInRow > maxDistance ifTrue: [ ^ 0 ].
		twoRowsBack := previousRow.
		previousRow := currentRow ].

	^ (previousRow last <= maxDistance)
		ifTrue: [ previousRow last ]
		ifFalse: [ 0 ]

]

{ #category : 'default values' }
CoToleranceFilter class >> defaultMinimumTokenSize [

	^ 2

	
]

{ #category : 'default values' }
CoToleranceFilter class >> defaultRate [

	^ 0.4

	
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

	self reset.
	self disable
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
CoToleranceFilter class >> scoreForCandidate: aCandidateString token: aTokenString caseSensitive: aBoolean tolerance: aTolerance [
	"Return zero if the candidate matches the token.
	
	"
	| candidate token maxSize maxEdits distance candidatePrefix|

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

	maxSize := token size.
	maxEdits := (aTolerance * maxSize) floor .
	maxEdits <= 0 ifTrue: [ ^ 0 ].
	
	"Fuzzy matching is applied only to the first n characters of the candidate,
 	 where n = min(token size, candidate size).
 	 The untyped suffix of the candidate is irrelevant to edit distance."
	candidatePrefix := candidate copyFrom: 1 to: (token size min: candidate size).

	distance := self
		damerauLevenshteinDistanceBetween: token
		and: candidatePrefix
		maxDistance: maxEdits.

	distance ifNil: [ ^ 0 ].
	^ distance asFloat / maxSize
]

{ #category : 'algorithms' }
CoToleranceFilter class >> shouldTryFuzzyFor: token candidate: candidate tolerance: aTolerance [ 

	(self enabled not or: [ aTolerance <= 0.0 ]) ifTrue: [ ^ false ].
	token isEmpty ifTrue: [ ^ false ].
	candidate isEmpty ifTrue: [ ^ false ].
	token size < self minimumTokenSize ifTrue: [ ^ false ].

	"Keep keyword arity stable"
	((token includes: $:) or: [ candidate includes: $: ]) ifTrue: [
		(token occurrencesOf: $:) = (candidate occurrencesOf: $:)
			ifFalse: [ ^ false ] ].
		
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
			ifFalse: [ scoreA < scoreB ] ].

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
