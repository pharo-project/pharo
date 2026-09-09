"
I'm a simple wrapper over a fetcher for workspace. This way we can add any fetcher in a workspace completion strategy.

Here is an example how we could do it for Coypu

```
CoASTHeuristicsResultSetBuilder >> coypu
	
	| entries f |
	entries := #(domenico beat hexBeat bpm upbeat rests banda braves asDirtyNotes)
		collect: [ :each | 
				| entry |
				entry := NECSelectorEntry contents: each node: astNode.
				entry fetcher: self.
				entry ].
	
	f := CoCollectionFetcher new collection: entries ;yourself.
	^ CoGenericWorkspaceHeuristic new fetcher: f 

CoASTHeuristicsResultSetBuilder >> messageHeuristic

	^ messageHeuristic ifNil: [ messageHeuristic := self newHeuristicBuilder
		add: CoSelfMessageHeuristic new;
		add: CoSuperMessageHeuristic new;
		add: self coypu;
		add: CoTypedReceiverMessageHeuristic new;
		add: CoInitializeInferencedMessageHeuristic new;
		add: CoLiteralMessageHeuristic new;
		add: CoGlobalVariableMessageHeuristic new;
		add: CoVariableWithTypeNameMessageHeuristic new;
		add: CoVariableValueMessageHeuristic new;
		add: CoUnknownMessageHeuristic new;
		build ]	
```
"
Class {
	#name : 'CoGenericWorkspaceHeuristic',
	#superclass : 'CoASTNodeFetcherHeuristic',
	#instVars : [
		'fetcher'
	],
	#category : 'HeuristicCompletion-Model-Heuristics',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Heuristics'
}

{ #category : 'requests' }
CoGenericWorkspaceHeuristic >> appliesForNode: aNode inContext: aContext [

	^ aContext isScripting
]

{ #category : 'requests' }
CoGenericWorkspaceHeuristic >> buildFetcherFor: aNode inContext: aContext [

	^ fetcher
]

{ #category : 'requests' }
CoGenericWorkspaceHeuristic >> fetcher: aFetcher [

	fetcher := aFetcher
]
