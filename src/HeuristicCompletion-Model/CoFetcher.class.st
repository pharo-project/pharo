"
I'm a kind of iterator (in the sense of GoF) that goes one by one over ""a stream"" of entities that can be consumed by the completion engine within a completion context.

I provide access to the elements I return one by one (using the next message) in a controlled manner. 
At the root, I define the architecture to yield one entry at a time. 
The next method invokes the hook method #entriesDo: with a generator yielding capability.
This way any use of #next will be done step by step. 

My subclasses implement the following method entriesDo: aBlock which defines the semantics of element fetching
Here is a simple example where a generic fetcher is used on a collection, and wrapped by a fetcher that avoids returning duplicate elements.

[[[
fetcher := AvoidRepeatedComplishonFetcher new
	complishonFetcher: (GenericComplishonFetcher onCollection: #( a b b a c )).
]]]
	
Here are two examples of definition

[[[
AvoidRepeatedComplishonFecther >> entriesDo: aBlock
	decoree
		entriesDo: [ :elem | 
			(self hasAlreadyReturned: elem)
				ifFalse: [ elements add: elem.
					aBlock value: elem ] ]
]]]

[[[
GenericComplishonFetcher >> entriesDo: aBlock

	collection do: aBlock
]]]



	

"
Class {
	#name : 'CoFetcher',
	#superclass : 'Object',
	#instVars : [
		'generator',
		'filter'
	],
	#category : 'HeuristicCompletion-Model-Core',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Core'
}

{ #category : 'composing' }
CoFetcher >> , anotherFetcher [

	^ CoFetcherSequence new
		add: self;
		add: anotherFetcher;
		yourself
]

{ #category : 'private' }
CoFetcher >> acceptsEntry: entry [

	^ filter accepts: entry
]

{ #category : 'testing' }
CoFetcher >> atEnd [

	^ self generator atEnd
]

{ #category : 'composing' }
CoFetcher >> collect: aBlockClosure [

	^ CoMappingFetcher new
		decoree: self;
		collectBlock: aBlockClosure;
		yourself
]

{ #category : 'enumerating' }
CoFetcher >> entriesDo: aBlock [
	"Key method for subclasses to define their semantics."

	^ self subclassResponsibility
]

{ #category : 'resetting' }
CoFetcher >> filter: newFilter [

	(newFilter isMoreNarrowThan: filter)
		ifFalse: [ self reset ].

	filter := newFilter
]

{ #category : 'private' }
CoFetcher >> generator [
	^ generator ifNil: [
		generator := Generator on: [ :g |
			self entriesDo: [ :entry |
				(self acceptsEntry: entry)
					ifTrue: [ g yield: entry ] ] ] ]
]

{ #category : 'initialization' }
CoFetcher >> initialize [
	super initialize.

	filter := CoFilter empty
]

{ #category : 'testing' }
CoFetcher >> isEmptyCompletionFetcher [

	^ false
]

{ #category : 'composing' }
CoFetcher >> narrowFilter: aFilter narrowKey: aKey [

	self filter: aFilter
]

{ #category : 'fetching' }
CoFetcher >> next [

	^ self generator next
]

{ #category : 'fetching' }
CoFetcher >> next: anInteger [

	^ Array new: anInteger streamContents: [ :stream | | total last |
		total := 0.
		[ (last := self next) ifNotNil: [ :each |
			stream nextPut: each.
			total := total + 1 ] ]
			doWhileTrue: [ total < anInteger and: [ last isNotNil ] ] ]
]

{ #category : 'resetting' }
CoFetcher >> reset [

	generator := nil
]

{ #category : 'composing' }
CoFetcher >> unnarrowFilter: aFilter narrowKey: aKey [

	self filter: aFilter
]

{ #category : 'enumerating' }
CoFetcher >> upToEnd [

	^ self generator upToEnd
]

{ #category : 'composing' }
CoFetcher >> withNarrowHistory [

	^ CoNarrowHistoryFetcher new
		decoree: self;
		yourself
]

{ #category : 'composing' }
CoFetcher >> withoutRepetition [

	^ CoAvoidRepeatedFetcher new
		decoree: self;
		yourself
]
