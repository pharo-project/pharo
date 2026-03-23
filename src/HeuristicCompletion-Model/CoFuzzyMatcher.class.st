Class {
	#name : 'CoFuzzyMatcher',
	#superclass : 'Object',
	#category : 'HeuristicCompletion-Model-Core',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Core'
}

{ #category : 'matching' }
CoFuzzyMatcher class >> damerauLevenshteinDistanceBetween: source and: target maxDistance: maxDistance [

	| previousRow currentRow twoRowsBack minInRow cost insertion deletion substitution value |
	source = target ifTrue: [ ^ 0 ].

	previousRow := Array new: target size + 1.
	1 to: (target size + 1) do: [ :j |
		previousRow at: j put: j - 1 ].
	twoRowsBack := nil.

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

		minInRow > maxDistance ifTrue: [ ^ nil ].
		twoRowsBack := previousRow.
		previousRow := currentRow ].

	^ (previousRow last <= maxDistance)
		ifTrue: [ previousRow last ]
		ifFalse: [ nil ]
]

{ #category : 'matching' }
CoFuzzyMatcher class >> scoreForCandidate: aCandidateString token: aTokenString caseSensitive: aBoolean tolerance: aTolerance [
	| candidate token maxSize maxEdits distance |
	candidate := aCandidateString asString.
	token := aTokenString asString.

	token isEmpty ifTrue: [ ^ 0.0 ].
	candidate = token ifTrue: [ ^ 0.0 ].

	candidate := aBoolean
		ifTrue: [ candidate ]
		ifFalse: [ candidate asLowercase ].
	token := aBoolean
		ifTrue: [ token ]
		ifFalse: [ token asLowercase ].

	(candidate beginsWith: token) ifTrue: [ ^ 0.0 ].
	(self shouldTryFuzzyFor: token candidate: candidate tolerance: aTolerance) ifFalse: [ ^ nil ].

	maxSize := token size max: candidate size.
	maxEdits := (aTolerance * maxSize) ceiling.
	maxEdits <= 0 ifTrue: [ ^ nil ].

	distance := self
		damerauLevenshteinDistanceBetween: token
		and: candidate
		maxDistance: maxEdits.

	distance ifNil: [ ^ nil ].
	^ distance asFloat / maxSize
]

{ #category : 'matching' }
CoFuzzyMatcher class >> shouldTryFuzzyFor: token candidate: candidate tolerance: aTolerance [

	| maxSize maxEdits |
	(CoCompletionTolerance enabled not or: [ aTolerance <= 0.0 ]) ifTrue: [ ^ false ].
	token isEmpty ifTrue: [ ^ false ].
	candidate isEmpty ifTrue: [ ^ false ].
	token size < CoCompletionTolerance minimumTokenSize ifTrue: [ ^ false ].

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
