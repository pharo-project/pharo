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
		and: [ aNode receiver isSuper ])
			or: [ aNode isMethod
				and: [ 
					"This heuristic is not valid for traits and nil subclasses"
					aContext completionClass notNil 
					and: [ aContext completionClass superclass notNil ] ] ]
]

{ #category : #requests }
CoSuperMessageHeuristic >> buildFetcherFor: aNode inContext: completionContext [
	| completionClass |
	
	"When in the playground, the completion class is not available.
	But self is bound to nil"
	completionClass := completionContext completionClass ifNil: [ nil class ].
	^ self newMessageInHierarchyFetcherForClass: completionClass superclass inASTNode: aNode
]
