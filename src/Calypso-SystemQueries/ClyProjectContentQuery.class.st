"
I return all contents of the project.
The actual objects are depends on project implementation. Look at ClyManagedProject for details.

"
Class {
	#name : 'ClyProjectContentQuery',
	#superclass : 'ClyTypedQuery',
	#category : 'Calypso-SystemQueries-Queries',
	#package : 'Calypso-SystemQueries',
	#tag : 'Queries'
}

{ #category : 'accessing' }
ClyProjectContentQuery class >> resultItemsType [

	^ Project
]

{ #category : 'execution' }
ClyProjectContentQuery >> buildResult: aQueryResult [

	| allItems |
	allItems := OrderedCollection new.

	scope packagesDo: [ :each | allItems add: each ].

	aQueryResult fillWith: allItems
]

{ #category : 'execution' }
ClyProjectContentQuery >> checkEmptyResult [

	scope packagesDo: [ :each | ^ false ].
	^ true
]

{ #category : 'execution' }
ClyProjectContentQuery >> collectMetadataOf: aQueryResult by: anEnvironmentPlugin [
]

{ #category : 'system changes' }
ClyProjectContentQuery >> isResult: aQueryResult affectedBy: aSystemAnnouncement [

	^scope includesPackagesAffectedBy: aSystemAnnouncement
]
