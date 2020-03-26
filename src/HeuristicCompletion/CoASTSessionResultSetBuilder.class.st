"
I am an extension to the AST Heuristic result set builder, that holds information during the session.
So, I enhance the work providing information of the last used / created selectors and classes.
"
Class {
	#name : #CoASTSessionResultSetBuilder,
	#superclass : #CoASTHeuristicsResultSetBuilder,
	#instVars : [
		'session'
	],
	#category : #'HeuristicCompletion-SystemIntegration'
}

{ #category : #heuristics }
CoASTSessionResultSetBuilder >> messageHeuristic [
	^ messageHeuristic ifNil: [ messageHeuristic := self newHeuristicBuilder
		add: (CoSessionMessagesHeuristic new
			session: session;
			yourself);
		add: CoUnknownMessageHeuristic new;
		build ]
]

{ #category : #accessing }
CoASTSessionResultSetBuilder >> session [
	^ session
]

{ #category : #accessing }
CoASTSessionResultSetBuilder >> session: anObject [
	session := anObject
]
