"
GenericComplishonFetcher I ensure that I will not return twice the same entity.
I am mainly used in the context of code completion of messages, although I can be used in other context.

For example, when looking to autocomplete messages for ==self==, an heuristic may choose to show the selectors in the class, then the selectors in the superclass, and so on, following the inheritance chain. However, in the class hierarchy we may observe redefined methods, so a naive fetcher will first show #redefined from the subclass and then #redefined again from the superclass.

I can be easily created by sending the #withoutRepetition message to another fetcher.

(GenericComplishonFetcher onCollection: aCollection) withoutRepetition.
"
Class {
	#name : 'CoAvoidRepeatedFetcher',
	#superclass : 'CoFetcherDecorator',
	#instVars : [
		'elements'
	],
	#category : 'HeuristicCompletion-Model-Core',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Core'
}

{ #category : 'enumerating' }
CoAvoidRepeatedFetcher >> entriesDo: aBlock [
	decoree
		entriesDo: [ :elem |
			(self hasAlreadyReturned: elem)
				ifFalse: [
					"As the NECEntry does not know how to compare I will enter the content as symbol"
					elements add: elem contents asSymbol.
					aBlock value: elem ] ]
]

{ #category : 'accessing' }
CoAvoidRepeatedFetcher >> hasAlreadyReturned: anEntry [

	^ elements includes: anEntry contents asSymbol
]

{ #category : 'initialization' }
CoAvoidRepeatedFetcher >> initialize [

	super initialize.
	elements := Set new
]

{ #category : 'resetting' }
CoAvoidRepeatedFetcher >> reset [

	super reset.
	elements := Set new
]
