"
I implement an heuristic for self message sends.
I provide a fetcher for messages for the hierarchy of self class superclass.

See my superclass for details.
"
Class {
	#name : #CoSuperMessageHeuristic,
	#superclass : #CoASTNodeFetcherHeuristic,
	#category : #'HeuristicCompletion-Model-Heuristics'
}

{ #category : #requests }
CoSuperMessageHeuristic >> appliesForNode: aNode inContext: aContext [

	^ (aNode isMessage
		and: [ aNode receiver isSuperVariable ])
			or: [ aNode isMethod
				and: [
					"This heuristic is not valid for traits and nil subclasses"
					aContext completionClass notNil
					and: [ aContext completionClass superclass notNil ] ] ]
]

{ #category : #requests }
CoSuperMessageHeuristic >> buildFetcherFor: aNode inContext: completionContext [
	| completionClass fetcher entry |

	"When in the playground, the completion class is not available.
	But self is bound to nil"
	completionClass := completionContext completionClass ifNil: [ nil class ].
	"In Traits, superclass is nil"
	completionClass superclass ifNil: [ ^ CoEmptyFetcher new ].
	
	fetcher := self newMessageInHierarchyFetcherForClass: completionClass superclass inASTNode: aNode.

	"Inject a synthetic fetcher with the expected full message send at first option because it is was is needed in most (>99%) situations"
	aNode methodNode ifNotNil: [ :methodNode |
		(completionClass superclass canUnderstand: methodNode selector) ifTrue: [ 
			entry := (NECSelectorEntry contents: methodNode selectorAndArgumentNames node: aNode) selector: methodNode selector.
			fetcher := (CoCollectionFetcher new collection: { entry }) , fetcher ] ].
	^ fetcher
]
