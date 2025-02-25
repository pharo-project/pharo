Class {
	#name : 'CoASTSessionResultSetBuilder',
	#superclass : 'CoASTHeuristicsResultSetBuilder',
	#instVars : [
		'session'
	],
	#category : 'HeuristicCompletion-Model-SystemIntegration',
	#package : 'HeuristicCompletion-Model',
	#tag : 'SystemIntegration'
}

{ #category : 'heuristics' }
CoASTSessionResultSetBuilder >> messageHeuristic [
	^ messageHeuristic ifNil: [ messageHeuristic := self newHeuristicBuilder
		add: (CoSessionMessagesHeuristic new
			session: session;
			yourself);
		add: CoUnknownMessageHeuristic new;
		build ]
]

{ #category : 'accessing' }
CoASTSessionResultSetBuilder >> session [
	^ session
]

{ #category : 'accessing' }
CoASTSessionResultSetBuilder >> session: anObject [
	session := anObject
]
