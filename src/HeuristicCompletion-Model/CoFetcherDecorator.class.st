Class {
	#name : #CoFetcherDecorator,
	#superclass : #CoFetcher,
	#instVars : [
		'decoree'
	],
	#category : #'HeuristicCompletion-Model-Core'
}

{ #category : #testing }
CoFetcherDecorator class >> isAbstract [

	^ self == CoFetcherDecorator
]

{ #category : #accessing }
CoFetcherDecorator >> decoree: anotherFetcher [
	
	decoree := anotherFetcher
]

{ #category : #resetting }
CoFetcherDecorator >> reset [

	super reset.
	decoree reset.
]
