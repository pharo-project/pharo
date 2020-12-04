"
I am simple processor that iterates on the implementors
"
Class {
	#name : #SptImplementorsProcessor,
	#superclass : #SptSpotterProcessor,
	#category : #'GT-Spotter-Processors-Processors'
}

{ #category : #'default-settings' }
SptImplementorsProcessor class >> defaultEnabled [

	^ false
]

{ #category : #settings }
SptImplementorsProcessor class >> hideInSettings [

	"I am hidden because the implementation in the unified processor is richer"
	^ true
]

{ #category : #accessing }
SptImplementorsProcessor class >> order [
	
	^ 10
]

{ #category : #accessing }
SptImplementorsProcessor class >> title [

	^ 'Implementors'
]

{ #category : #filtering }
SptImplementorsProcessor >> newTextFilteringSource [

	^ SptSourceFactory current implementorsSubstringSource
]
