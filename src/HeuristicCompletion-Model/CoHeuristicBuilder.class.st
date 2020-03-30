Class {
	#name : #CoHeuristicBuilder,
	#superclass : #Object,
	#instVars : [
		'heuristics'
	],
	#category : #'HeuristicCompletion-Model-SystemIntegration'
}

{ #category : #adding }
CoHeuristicBuilder >> add: anHeuristic [
	
	heuristics add: anHeuristic
]

{ #category : #building }
CoHeuristicBuilder >> build [

	^ heuristics reduceRight: [ :left :right | left next: right. left ]
]

{ #category : #initialization }
CoHeuristicBuilder >> initialize [
	super initialize.
	heuristics := OrderedCollection new
]
