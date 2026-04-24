"
I am a fetcher that gets all message selectors. 
I use the SystemNavigation to get all selectors and then filtering them.
See the #entriesDo:
"
Class {
	#name : 'CoGlobalSelectorFetcher',
	#superclass : 'CoGlobalFetcher',
	#category : 'HeuristicCompletion-Model-Fetchers',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Fetchers'
}

{ #category : 'enumerating' }
CoGlobalSelectorFetcher >> entriesDo: aBlock [

	astNode ifNotNil: [ :node |
			node parent ifNotNil: [ :parent |
					(parent isMessage and: [ parent isKeyword ]) ifTrue: [
							self systemNavigation allSelectorsStartingWith: parent selector , self effectivePrefix do: [ :e |
									aBlock value: (CoSelectorEntry new
											 displayedContents:
												 '(' , (e copyFrom: 1 to: parent selector size) , ')' , (e copyFrom: parent selector size + 1 to: e size);
											 contents: (e copyFrom: parent selector size + 1 to: e size);
											 node: astNode;
											 fetcherName: self class name;
											 selector: e) ] ] ] ].
	self systemNavigation allSelectorsStartingWith: self effectivePrefix do: [ :e |
			aBlock value: ((CoSelectorEntry contents: e node: astNode)
					 fetcherName: self class name;
					 yourself) ]
]
