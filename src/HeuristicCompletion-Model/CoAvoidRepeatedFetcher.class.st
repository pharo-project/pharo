Class {
	#name : #CoAvoidRepeatedFetcher,
	#superclass : #CoFetcherDecorator,
	#instVars : [
		'elements'
	],
	#category : #'HeuristicCompletion-Model-Core'
}

{ #category : #enumerating }
CoAvoidRepeatedFetcher >> entriesDo: aBlock [
	decoree
		entriesDo: [ :elem | 
			(self hasAlreadyReturned: elem)
				ifFalse: [ 
					"As the NECEntry does not know how to compare I will enter the content as symbol"
					elements add: elem contents asSymbol.
					aBlock value: elem ] ]
]

{ #category : #accessing }
CoAvoidRepeatedFetcher >> hasAlreadyReturned: anEntry [

	^ elements includes: anEntry contents asSymbol
]

{ #category : #initialization }
CoAvoidRepeatedFetcher >> initialize [

	super initialize.
	elements := Set new.
]

{ #category : #resetting }
CoAvoidRepeatedFetcher >> reset [

	super reset.
	elements := Set new.
]
