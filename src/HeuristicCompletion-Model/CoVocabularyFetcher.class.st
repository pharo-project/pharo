Class {
	#name : 'CoVocabularyFetcher',
	#superclass : 'CoFetcher',
	#instVars : [
		'mapping'
	],
	#category : 'HeuristicCompletion-Model-NewFetchers',
	#package : 'HeuristicCompletion-Model',
	#tag : 'NewFetchers'
}

{ #category : 'enumerating' }
CoVocabularyFetcher >> acceptsEntry: anEntry [
	'.' traceCr.
	self halt.
	^ true
]

{ #category : 'enumerating' }
CoVocabularyFetcher >> entriesDo: aBlock [
	
	| item |
	item := mapping at: filter completionString ifAbsent: [ nil ].
	item ifNotNil: [ aBlock value: ((NECEntry new contents: item) fetcher: self; yourself) ].
	
]

{ #category : 'initialization' }
CoVocabularyFetcher >> fillSomePredefinedVocabulary [

	mapping at: 'OC' put: 'OrderedCollection new.'
]

{ #category : 'initialization' }
CoVocabularyFetcher >> initialize [ 

	super initialize.
	mapping := Dictionary new. 
	self fillSomePredefinedVocabulary
]
