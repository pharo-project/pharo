"
I am a mapping complishon fetcher. I decorate a complishon fetcher and transform its results to those ones using the collectBlock block.
I can be created by sending the #collect: message to an existing fetcher.

(GenericComplishonFetcher onCollection: aCollection) collect [ :each | each * 2 ]
"
Class {
	#name : 'CoMappingFetcher',
	#superclass : 'CoFetcherDecorator',
	#instVars : [
		'collectBlock'
	],
	#category : 'HeuristicCompletion-Model-Core',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Core'
}

{ #category : 'accessing' }
CoMappingFetcher >> collectBlock: aBlockClosure [

	collectBlock := aBlockClosure
]

{ #category : 'enumerating' }
CoMappingFetcher >> entriesDo: aBlockClosure [

	decoree entriesDo: [ :each | aBlockClosure value: (collectBlock value: each) ]
]
