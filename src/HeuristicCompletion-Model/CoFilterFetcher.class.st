Class {
	#name : #CoFilterFetcher,
	#superclass : #CoFetcherDecorator,
	#instVars : [
		'filterBlock'
	],
	#category : #'HeuristicCompletion-Model-Core'
}

{ #category : #enumerating }
CoFilterFetcher >> entriesDo: aBlockClosure [
	decoree
		entriesDo: [ :each | 
			(filterBlock value: each)
				ifTrue: [ aBlockClosure value: each ] ]
]

{ #category : #accessing }
CoFilterFetcher >> filterBlock: aBlockClosure [ 
	
	filterBlock := aBlockClosure
]

{ #category : #composing }
CoFilterFetcher >> narrowFilterBlock: aBlock [

	self filterBlock: aBlock
]

{ #category : #composing }
CoFilterFetcher >> select: aBlockClosure [

	"Do not filter over a filter.
	Instead, create a new filter over the original fetcher"
	^ self withoutFilter select: aBlockClosure
]

{ #category : #composing }
CoFilterFetcher >> withoutFilter [
	
	^ decoree
]
