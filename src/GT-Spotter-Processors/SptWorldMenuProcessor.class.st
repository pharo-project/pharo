"
I am a simple processor that returns all the world menu entries. 
"
Class {
	#name : #SptWorldMenuProcessor,
	#superclass : #SptSpotterProcessor,
	#category : #'GT-Spotter-Processors-Processors'
}

{ #category : #'default-settings' }
SptWorldMenuProcessor class >> defaultEnabled [

	^ true
]

{ #category : #accessing }
SptWorldMenuProcessor class >> order [
	
	^ 9
]

{ #category : #accessing }
SptWorldMenuProcessor class >> title [

	^ 'Menu'
]

{ #category : #filtering }
SptWorldMenuProcessor >> newTextFilteringSource [

	^ ((SptGeneratorBlockIterator
		on: [ :aValuable | self currentWorld worldMenu withAllLeafItemsDo: aValuable ])
		collect: [ :e | SptWorldMenuEntry on: e ]) asSubstringFilter
]

{ #category : #configuration }
SptWorldMenuProcessor >> showForEmptyQuery [

	^ true
]
