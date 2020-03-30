Class {
	#name : #CoUnknownMessageHeuristic,
	#superclass : #CoASTNodeFetcherHeuristic,
	#category : #'HeuristicCompletion-Model-Heuristics'
}

{ #category : #requests }
CoUnknownMessageHeuristic >> appliesForNode: aNode inContext: aContext [

	^ aNode isMethod or: [ aNode isMessage ]
]

{ #category : #requests }
CoUnknownMessageHeuristic >> buildFetcherFor: aNode inContext: completionContext [
	"This is the main heuristic used for those messages where we cannot guess the type.
	I can be used also after more fine heuristics, to complete guessed heuristics that cannot be trusted too much, like variable names"

	"If the context allows it, autocomplete first messages in the current package, then the rest in the system"
	| fetcher |
	fetcher := (CoCollectionFetcher onCollection: Symbol selectorTable)
		collect: [ :e | NECSelectorEntry contents: e node: self ].
	completionContext completionClass ifNotNil: [
		fetcher := (CoPackageImplementedMessagesFetcher new
			completionPackage: completionContext completionClass package;
			yourself), fetcher
	].
	^ fetcher
]
