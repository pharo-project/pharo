"
I am an object keeping code completion statistics.
I store two collection of events: accepted events and cancelled events.
Both events contain information about the completion context (class, is workspace...).
Accepted events contain also the text accepted, the index, and the heuristic.

I provide methods to access and query the statistics.

I have a singleton instance for convenience.
"
Class {
	#name : 'CoStatistics',
	#superclass : 'Object',
	#instVars : [
		'acceptedEntries',
		'cancelledCompletions'
	],
	#classVars : [
		'UniqueInstance'
	],
	#category : 'HeuristicCompletion-Model-Statistics',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Statistics'
}

{ #category : 'class initialization' }
CoStatistics class >> reset [

	UniqueInstance := nil
]

{ #category : 'singleton' }
CoStatistics class >> uniqueInstance [

	^ UniqueInstance ifNil: [
		UniqueInstance := self new ]
]

{ #category : 'accessing' }
CoStatistics >> acceptedEntries [

	^ acceptedEntries
]

{ #category : 'accessing' }
CoStatistics >> averageTypedCharacters [

	| count total |
	count := 0.
	total := 0.
	acceptedEntries do: [ :entries |
		count := count + entries size.
		total := total + (entries sum: [ :entry | entry numberOfTypedCharacter ]).
	].

	^ count = 0
		ifTrue: [ 0 ]
		ifFalse: [ total / count ]
]

{ #category : 'accessing' }
CoStatistics >> averageTypedCharactersAt: anIndex [

	| count total entries |
	count := total := 0.
	entries := acceptedEntries at: anIndex ifAbsent: [ ^ 0 ].
	count := count + entries size.
	total := entries sum: [ :entry | entry numberOfTypedCharacter ].
	^ count = 0
		ifTrue: [ 0 ]
		ifFalse: [ total / count ]
]

{ #category : 'accessing' }
CoStatistics >> entriesAtIndex: anInteger [

	^ acceptedEntries at: anInteger ifAbsentPut: [ CoEntryAcceptedEvent new ]
]

{ #category : 'accessing' }
CoStatistics >> entriesGeneratedBy: aSource [

	^ acceptedEntries sum: [ :entries |
		entries count: [ :entry | entry source = aSource ] ]
]

{ #category : 'querying' }
CoStatistics >> groupBy: aBlockClosure [

	^ acceptedEntries gather: [ :entriesByIndex |
		entriesByIndex select: aBlockClosure ]
]

{ #category : 'initialization' }
CoStatistics >> initialize [

	super initialize.
	acceptedEntries := Dictionary new.
	cancelledCompletions := OrderedCollection new
]

{ #category : 'events' }
CoStatistics >> logAcceptEvent: anEntry [

	| entries |
	entries := acceptedEntries at: anEntry index ifAbsentPut: [ OrderedCollection new ].
	entries add: anEntry
]

{ #category : 'events' }
CoStatistics >> logCancelCompletionFromTypedToken: typedToken inClass: aClassName withDynamicExtent: isDynamicExtent [

	self logCancelEvent: (CoCompletionCancelledEvent new
		typedToken: typedToken;
		contextClassName: aClassName;
		isDynamicContext: isDynamicExtent;
		yourself)
]

{ #category : 'events' }
CoStatistics >> logCancelEvent: aCancelEntry [

	cancelledCompletions add: aCancelEntry
]

{ #category : 'accessing' }
CoStatistics >> numberOfCancels [

	^ cancelledCompletions size
]

{ #category : 'accessing' }
CoStatistics >> numberOfCancelsByClass: aClassName [

	^ cancelledCompletions count: [ :e | e contextClassName = aClassName ]
]

{ #category : 'accessing' }
CoStatistics >> numberOfCancelsByClassCompletions: cancelledCompletionsInClass [

	^ cancelledCompletionsInClass sum: [ :e |
		e ifNil: [ 0 ] ifNotNil: [ e size ] ]
]
