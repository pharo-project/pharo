Class {
	#name : 'CoSnippetFetcher',
	#superclass : 'CoFetcher',
	#classVars : [
		'Mapping'
	],
	#category : 'HeuristicCompletion-Model-NewFetchers',
	#package : 'HeuristicCompletion-Model',
	#tag : 'NewFetchers'
}

{ #category : 'declaring snippets' }
CoSnippetFetcher class >> fillSomePredefinedVocabulary [

	self replace: 'OC' bySnippet: 'OrderedCollection new.'
	
]

{ #category : 'class initialization' }
CoSnippetFetcher class >> initialize [ 
	<script>
	
	Mapping := Dictionary new. 
	self fillSomePredefinedVocabulary
]

{ #category : 'declaring snippets' }
CoSnippetFetcher class >> replace: aShortString bySnippet: aSnippet [
	"Add the fact that when the user type aShortString, it is replaced by aSnippet."
	
	Mapping at: aShortString put: aSnippet
]

{ #category : 'enumerating' }
CoSnippetFetcher >> entriesDo: aBlock [
	
	| item |
	item := Mapping at: filter completionString ifAbsent: [ nil ].
	item ifNotNil: [ aBlock value: ((CoSnippetEntry new
														shortString: filter completionString ;
														contents: item) fetcherName: self class name; yourself) ].
	
]
