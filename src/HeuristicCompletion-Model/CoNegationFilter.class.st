Class {
	#name : #CoNegationFilter,
	#superclass : #CoFilter,
	#instVars : [
		'negatedFilter'
	],
	#category : #'HeuristicCompletion-Model-Core'
}

{ #category : #testing }
CoNegationFilter >> accepts: aCandidate [
	
	^ (negatedFilter accepts: aCandidate) not
]

{ #category : #testing }
CoNegationFilter >> negated [

	^ negatedFilter
]

{ #category : #accessing }
CoNegationFilter >> negatedFilter [

	^ negatedFilter
]

{ #category : #accessing }
CoNegationFilter >> negatedFilter: anObject [

	negatedFilter := anObject
]
