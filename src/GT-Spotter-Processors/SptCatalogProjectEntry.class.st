"
I wrap a CatalogProject entry, to modify its behavior if required for the spotter
"
Class {
	#name : #SptCatalogProjectEntry,
	#superclass : #SptEntry,
	#category : #'GT-Spotter-Processors-Entries'
}

{ #category : #converting }
SptCatalogProjectEntry >> asString [ 

	^ content name
]

{ #category : #evaluating }
SptCatalogProjectEntry >> doEvaluate [
	(UIManager default
		confirm: 'Would you like to install ' , content name , '?')
		ifFalse: [ ^ self ].

	content installStableVersion.
	UIManager inform: content name , ' installed'
]

{ #category : #accessing }
SptCatalogProjectEntry >> icon [

	^ self iconNamed: #catalog
]
