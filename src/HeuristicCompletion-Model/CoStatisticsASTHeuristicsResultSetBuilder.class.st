"
I am a result set builder that builds heuristics using statistic support
"
Class {
	#name : #CoStatisticsASTHeuristicsResultSetBuilder,
	#superclass : #CoASTHeuristicsResultSetBuilder,
	#instVars : [
		'statistics'
	],
	#category : #'HeuristicCompletion-Model-Statistics'
}

{ #category : #heuristics }
CoStatisticsASTHeuristicsResultSetBuilder >> newHeuristicBuilder [

	^ CoStatisticsHeuristicBuilder new
		statistics: statistics;
		yourself
]

{ #category : #accessing }
CoStatisticsASTHeuristicsResultSetBuilder >> statistics [
	^ statistics
]

{ #category : #accessing }
CoStatisticsASTHeuristicsResultSetBuilder >> statistics: anObject [
	statistics := anObject
]
