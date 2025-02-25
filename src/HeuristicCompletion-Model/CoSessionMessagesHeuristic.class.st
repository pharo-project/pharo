"
I am an heuristic that autocompletes messages based on a session vocabulary.
"
Class {
	#name : 'CoSessionMessagesHeuristic',
	#superclass : 'CoASTNodeFetcherHeuristic',
	#instVars : [
		'session'
	],
	#category : 'HeuristicCompletion-Model-SessionHeuristic',
	#package : 'HeuristicCompletion-Model',
	#tag : 'SessionHeuristic'
}

{ #category : 'requests' }
CoSessionMessagesHeuristic >> appliesForNode: aNode inContext: aContext [

	^ aNode isMethod or: [ aNode isMessage ]
]

{ #category : 'requests' }
CoSessionMessagesHeuristic >> buildFetcherFor: aMessageNode inContext: aContext [

	^ CoCollectionFetcher onCollection: session words
]

{ #category : 'accessing' }
CoSessionMessagesHeuristic >> session [
	^ session
]

{ #category : 'accessing' }
CoSessionMessagesHeuristic >> session: anObject [
	session := anObject
]
