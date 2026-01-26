"
I am a fetcher that gets all message selectors. 
I use the SystemNavigation to get all selectors and then filtering them.
See the #entriesDo:
"
Class {
	#name : 'CoGlobalSelectorFetcher',
	#superclass : 'CoGlobalFetcher',
	#instVars : [
		'fullCompletionString'
	],
	#category : 'HeuristicCompletion-Model-Fetchers',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Fetchers'
}

{ #category : 'enumerating' }
CoGlobalSelectorFetcher >> acceptsEntry: anEntry [
	"Here we do not know to filter the entry because the entry will not correspond to the token filter because it is about the corresponding snippet."
	
	^ true
]

{ #category : 'enumerating' }
CoGlobalSelectorFetcher >> entriesDo: aBlock [
	"We try two approaches
	
	- the first one is to see if we can take advantage that we are in the beginning of a keyword message.
	
	For example imagine that the user 
			
			x at: a p|  
			
	we are trying to propose at:put:.		 
			
	- the last one is to look globally in the method implementors and try to match. 		
	"
	' #' traceCr.
	astNode ifNotNil: [ :node |
			node parent ifNotNil: [ :parent | "Try first to continue the parent keyword message"
					(parent isMessage and: [ parent isKeyword ]) ifTrue: [
							fullCompletionString := parent selector , filter completionString.
							self systemNavigation allSelectorsStartingWith: fullCompletionString do: [ :e |
									e traceCr.
									aBlock value: ((NECSelectorEntry
											  contents:
												  '(' , (e copyFrom: 1 to: parent selector size) , ')' , (e copyFrom: parent selector size + 1 to: e size)
											  node: astNode)
											 fetcher: self;
											 selector: e) ] ] ] ]. "Otherwise, just go wide"
	self systemNavigation allSelectorsStartingWith: filter completionString do: [ :e |
			aBlock value: ((NECSelectorEntry contents: e node: astNode)
					 fetcher: self;
					 yourself) ]
]
