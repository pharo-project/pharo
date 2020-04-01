"
I am a processor to get all the windows open in the world. It filters out the top most window
"
Class {
	#name : #SptWindowsProcessor,
	#superclass : #SptCollectionBasedProcessor,
	#category : #'GT-Spotter-Processors-Processors'
}

{ #category : #'default-settings' }
SptWindowsProcessor class >> defaultEnabled [

	^ true
]

{ #category : #accessing }
SptWindowsProcessor class >> order [
	
	^ 5
]

{ #category : #accessing }
SptWindowsProcessor class >> title [

	^ 'Windows'
]

{ #category : #filtering }
SptWindowsProcessor >> collection [

	^ (self currentWorld submorphs
		select: [ :window | window isKindOf: SystemWindow ])
		ifNotEmpty: [ :aCollection | aCollection allButFirst ]
]

{ #category : #filtering }
SptWindowsProcessor >> newEntryOn: anElement [

	^ SptWindowEntry on: anElement
]

{ #category : #configuration }
SptWindowsProcessor >> showForEmptyQuery [

	^ true
]
