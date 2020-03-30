"
I am an heuristic that autocompletes messages based on a session vocabulary.
"
Class {
	#name : #CoSessionMessagesHeuristic,
	#superclass : #CoASTNodeFetcherHeuristic,
	#instVars : [
		'session'
	],
	#category : #'HeuristicCompletion-Model-SessionHeuristic'
}

{ #category : #requests }
CoSessionMessagesHeuristic >> appliesForNode: aNode inContext: aContext [

	^ aNode isMethod or: [ aNode isMessage ]
]

{ #category : #requests }
CoSessionMessagesHeuristic >> buildFetcherFor: aRBMessageNode inContext: aContext [

	^ CoCollectionFetcher onCollection: session words
]

{ #category : #accessing }
CoSessionMessagesHeuristic >> session [
	^ session
]

{ #category : #accessing }
CoSessionMessagesHeuristic >> session: anObject [
	session := anObject
]
