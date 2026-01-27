"
I'm a fetcher that maps a short string to a code snippet. 
This way user can type for example `OC` and get `OrderedCollection new.`
"
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
CoSnippetFetcher >> acceptsEntry: anEntry [
	"Here we do not know to filter the entry because the entry will not correspond to the token filter because it is about the corresponding snippet."

	^ true
]

{ #category : 'enumerating' }
CoSnippetFetcher >> entriesDo: aBlock [
	
	| item |
	item := Mapping at: filter completionString ifAbsent: [ nil ].
	item ifNotNil: [ aBlock value: ((NECSnippetEntry new
														shortString: filter completionString ;
														contents: item) fetcher: self; yourself) ].
	
]
