"
I am simple processor that iterates on the classes
"
Class {
	#name : #SptClassProcessor,
	#superclass : #SptSpotterProcessor,
	#category : #'GT-Spotter-Processors-Processors'
}

{ #category : #'default-settings' }
SptClassProcessor class >> defaultEnabled [

	^ false
]

{ #category : #settings }
SptClassProcessor class >> hideInSettings [

	"I am hidden because the implementation in the unified processor is richer"
	^ true
]

{ #category : #accessing }
SptClassProcessor class >> order [
	
	^ 10
]

{ #category : #accessing }
SptClassProcessor class >> title [

	^ 'Classes'
]

{ #category : #filtering }
SptClassProcessor >> newTextFilteringSource [

	^ SptSourceFactory current classesSubstringSource
]
