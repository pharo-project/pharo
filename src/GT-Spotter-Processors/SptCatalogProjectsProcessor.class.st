"
I am processor that gets all the projects from the Catalog.
However, this operation is quite expensive so this is not enabled by default.
"
Class {
	#name : #SptCatalogProjectsProcessor,
	#superclass : #SptCollectionBasedProcessor,
	#category : #'GT-Spotter-Processors-Processors'
}

{ #category : #'default-settings' }
SptCatalogProjectsProcessor class >> defaultEnabled [

	^ false
]

{ #category : #accessing }
SptCatalogProjectsProcessor class >> order [
	
	^ 90
]

{ #category : #accessing }
SptCatalogProjectsProcessor class >> title [

	^ 'Catalog Projects'
]

{ #category : #filtering }
SptCatalogProjectsProcessor >> collection [

	^ CatalogProvider default projects
]

{ #category : #filtering }
SptCatalogProjectsProcessor >> newEntryOn: anElement [

	^ SptCatalogProjectEntry on: anElement
]
