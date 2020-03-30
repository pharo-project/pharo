Class {
	#name : #CoMappingFetcher,
	#superclass : #CoFetcherDecorator,
	#instVars : [
		'collectBlock'
	],
	#category : #'HeuristicCompletion-Model-Core'
}

{ #category : #accessing }
CoMappingFetcher >> collectBlock: aBlockClosure [ 
	
	collectBlock := aBlockClosure
]

{ #category : #enumerating }
CoMappingFetcher >> entriesDo: aBlockClosure [

	decoree entriesDo: [ :each | aBlockClosure value: (collectBlock value: each) ]
]
