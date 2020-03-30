Class {
	#name : #CoFetcher,
	#superclass : #Object,
	#instVars : [
		'generator'
	],
	#category : #'HeuristicCompletion-Model-Core'
}

{ #category : #composing }
CoFetcher >> , anotherFetcher [ 

	^ CoFetcherSequence new
		add: self;
		add: anotherFetcher;
		yourself
]

{ #category : #testing }
CoFetcher >> atEnd [
	
	^ self generator atEnd
]

{ #category : #composing }
CoFetcher >> collect: aBlockClosure [ 
	
	^ CoMappingFetcher new
		decoree: self;
		collectBlock: aBlockClosure;
		yourself
]

{ #category : #enumerating }
CoFetcher >> entriesDo: aBlock [
	"Key method for subclasses to define their semantics."
	
	^ self subclassResponsibility
]

{ #category : #private }
CoFetcher >> generator [
	^ generator ifNil: [ 
		generator := Generator on: [ :g | 
			self entriesDo: [ :entry | g yield: entry ] ] ]
]

{ #category : #testing }
CoFetcher >> isEmptyCompletionFetcher [
	
	^ false
]

{ #category : #composing }
CoFetcher >> narrowFilterBlock: aBlockClosure [ 
	
	^ self select: aBlockClosure
]

{ #category : #fetching }
CoFetcher >> next [
	
	^ self generator next
]

{ #category : #fetching }
CoFetcher >> next: anInteger [ 
	
	^ Array new: anInteger streamContents: [ :stream | | total last |
		total := 0.
		[ (last := self next) ifNotNil: [ :each | 
			stream nextPut: each.
			total := total + 1 ] ]
			doWhileTrue: [ total < anInteger and: [ last notNil ] ] ]
]

{ #category : #resetting }
CoFetcher >> reset [
	
	generator := nil
]

{ #category : #composing }
CoFetcher >> select: aBlockClosure [ 
	
	^ CoFilterFetcher new
		decoree: self;
		filterBlock: aBlockClosure;
		yourself
]

{ #category : #enumerating }
CoFetcher >> upToEnd [
	
	^ self generator upToEnd
]

{ #category : #composing }
CoFetcher >> withoutFilter [
	
	^ self
]

{ #category : #composing }
CoFetcher >> withoutRepetition [

	^ CoAvoidRepeatedFetcher new
		decoree: self;
		yourself
]
