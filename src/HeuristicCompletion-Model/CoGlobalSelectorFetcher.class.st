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
	"We try two approaches
	
	- the first one is to see if we can take advantage that we are in the beginning of a keyword message.
	
	For example imagine that the user 
			
			x at: a p|  
			
	we are trying to propose at:put:.		 
			
	- the last one is to look globally in the method implementors and try to match. 		
	"
	
	| size compl smallerToken |
	astNode ifNotNil: [ :node |
		node parent ifNotNil: [ :parent | 
			"Try first to continue the parent keyword message"
			(parent isMessage and: [ parent isKeyword ]) ifTrue: [
				compl := parent selector. 
				size := compl size.
				smallerToken := (self tokenToCheckFrom: compl withSize: size).
				self systemNavigation 
					allSelectorsStartingWith: smallerToken
					do: [ :e |
						aBlock value: (CoSelectorEntry new
							displayedContents:
								'(' , (e copyFrom: 1 to: smallerToken size) , ')' , (e copyFrom: smallerToken size + 1 to: e size);
							contents: (e copyFrom:  smallerToken size + 1 to: e size);
							node: astNode;
							fetcherName: self class name;
							selector: e) ] ] ] ]. 
	"Otherwise, just go wide"
	compl := filter completionString.
	size := compl size.
	
	"here we are trying to guide less the collection of possible matches so that the tolerance can then play its role
	Indeed if we are looking for SpPresenter and type SpPrez looking for SpPrez* will not give SpPresenter so the idea if to reduce the 
	length of the prefix so that at the end of the prefix we get more chances of copiing with typos."
	self systemNavigation 
		allSelectorsStartingWith: 
			(self tokenToCheckFrom: compl withSize: size)   
		do: [ :e |
				aBlock value: ((CoSelectorEntry contents: e node: astNode)
					 fetcherName: self class name;
					 yourself) ]
]
