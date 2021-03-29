"
I am a simple processor that returns all the world menu entries. 
"
Class {
	#name : #StWorldMenuProcessor,
	#superclass : #StSpotterProcessor,
	#category : #'NewTools-Spotter-Processors-Processors'
}

{ #category : #'default-settings' }
StWorldMenuProcessor class >> defaultEnabled [

	^ true
]

{ #category : #accessing }
StWorldMenuProcessor class >> order [
	
	^ 90
]

{ #category : #accessing }
StWorldMenuProcessor class >> title [

	^ 'Menu'
]

{ #category : #filtering }
StWorldMenuProcessor >> newTextFilteringSource [

	^ ((StGeneratorBlockIterator
		on: [ :aValuable | self currentWorld worldMenu withAllLeafItemsDo: aValuable ])
		collect: [ :e | StWorldMenuEntry on: e ]) asSubstringFilter
]

{ #category : #configuration }
StWorldMenuProcessor >> showForEmptyQuery [

	^ true
]
