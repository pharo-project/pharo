"
I implement the lookup of #Senders and the Meta + n binding.

"
Class {
	#name : #SptSendersProcessor,
	#superclass : #SptSpotterProcessor,
	#category : #'GT-Spotter-Processors-Processors'
}

{ #category : #'default-settings' }
SptSendersProcessor class >> defaultEnabled [

	^ true
]

{ #category : #accessing }
SptSendersProcessor class >> order [
	
	^ 15
]

{ #category : #accessing }
SptSendersProcessor class >> title [
	
	^ 'Senders'
]

{ #category : #filtering }
SptSendersProcessor >> executeQuery [

	(query categoriesSelected includes: 'Senders')
		ifFalse: [ ^ self ].

	^ super executeQuery 
]

{ #category : #'key-bindings' }
SptSendersProcessor >> installKeymappingsOn: aGTSpotterMorph [

	aGTSpotterMorph
		bindKeyCombination: $n meta
		ofProcessor: self
		toAction: [ aGTSpotterMorph onKeyProcessor: self ]
]

{ #category : #filtering }
SptSendersProcessor >> newTextFilteringSource [

	^ SptSendersIterator new 
]
