"
I implement an heuristic for messages sent to variables whose value is available in the context, for example in a debugger, workspace or inspector.
If there is a value for the variable, create a fetcher for messages of its class.

See my superclass for details.
"
Class {
	#name : 'CoVariableValueMessageHeuristic',
	#superclass : 'CoVariableMessageHeuristic',
	#category : 'HeuristicCompletion-Model-Heuristics',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Heuristics'
}

{ #category : 'requests' }
CoVariableValueMessageHeuristic >> buildFetcherFor: aMessageNode inContext: aContext [

	"Try to fetch the real value of the variable from the context"
	self
		valueOfVariable: aMessageNode receiver name
		inContext: aContext
		ifPresent: [ :value |
			^ self
				newMessageInHierarchyFetcherForClass: value class
				inASTNode: aMessageNode ].

	^ CoEmptyFetcher new
]

{ #category : 'requests' }
CoVariableValueMessageHeuristic >> valueOfVariable: aName inContext: completionContext ifPresent: aBlock [

	| binding |
	"In the debugger, the binding values comes in the doItContext.
	If we find in the doItContext has a variable with the given name, use it, otherwise ignore it."
	(completionContext doItContext ifNotNil: [ :context |
		(context lookupVar: aName)
			ifNotNil: [:var | var readInContext: context ] ])
		ifNotNil: [ :value | ^ aBlock value: value ].

	"In the playground, the binding values comes in the bindings of the requestor.
	If we find in the requestor a binding with the given name, use it, otherwise ignore it."

	"We check using hasBindingOf: to not create bindings that are not there"
	(completionContext doItRequestor hasBindingOf: aName) ifFalse:  [ ^ self ].

	binding := completionContext doItRequestor bindingOf: aName.
	^ aBlock value: binding value
]
