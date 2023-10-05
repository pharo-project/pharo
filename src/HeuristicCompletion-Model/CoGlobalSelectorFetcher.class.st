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

	astNode ifNotNil: [ :node | node parent ifNotNil: [ :parent | "Try first to continue the parent keyword message"
		(parent isMessage and: [ parent isKeyword ]) ifTrue: [
			self systemNavigation
				allSelectorsStartingWith:
				parent selector , filter completionString
				do: [ :e |
					aBlock value: ((NECSelectorEntry
							 contents: (e copyFrom: parent selector size + 1 to: e size)
							 node: astNode)
							 selector: e)
							 ] ] ] ].

	"Otherwise, just go wide"
	self systemNavigation
		allSelectorsStartingWith: filter completionString
		do: [ :e |
		aBlock value: (NECSelectorEntry contents: e node: astNode) ]
]
