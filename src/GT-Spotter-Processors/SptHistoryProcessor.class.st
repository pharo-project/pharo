"
I am a processor that shows all the entries in the spotter history.
"
Class {
	#name : #SptHistoryProcessor,
	#superclass : #SptCollectionBasedProcessor,
	#category : #'GT-Spotter-Processors-Processors'
}

{ #category : #'default-settings' }
SptHistoryProcessor class >> defaultEnabled [

	^ true
]

{ #category : #accessing }
SptHistoryProcessor class >> order [
	
	^ 8
]

{ #category : #accessing }
SptHistoryProcessor class >> title [

	^ 'History'
]

{ #category : #filtering }
SptHistoryProcessor >> collection [

	^ GTSpotter history reject: [ :e | e isNil ]
]

{ #category : #filtering }
SptHistoryProcessor >> newEntryOn: anElement [

	^ anElement
]

{ #category : #configuration }
SptHistoryProcessor >> showForEmptyQuery [

	^ true
]
