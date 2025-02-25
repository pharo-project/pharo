"
I am a query of all projects from given scope.
The project managers should return a subclass of myself.

"
Class {
	#name : 'ClyAllProjectsQuery',
	#superclass : 'ClyTypedQuery',
	#category : 'Calypso-SystemQueries-Queries',
	#package : 'Calypso-SystemQueries',
	#tag : 'Queries'
}

{ #category : 'items type' }
ClyAllProjectsQuery class >> resultItemsType [

	^ Project
]

{ #category : 'execution' }
ClyAllProjectsQuery >> buildResult: aQueryResult [

	| packages |
	packages := OrderedCollection new.

	scope projectsDo: [ :each | packages add: each ].

	aQueryResult fillWith: packages
]

{ #category : 'execution' }
ClyAllProjectsQuery >> checkEmptyResult [

	scope projectsDo: [ :each | ^ false ].

	^ true
]

{ #category : 'execution' }
ClyAllProjectsQuery >> collectMetadataOf: aQueryResult by: anEnvironmentPlugin [

	anEnvironmentPlugin collectMetadataOfProjects: aQueryResult
]

{ #category : 'execution' }
ClyAllProjectsQuery >> description [

	^ 'Projects'
]

{ #category : 'execution' }
ClyAllProjectsQuery >> isResult: aQueryResult affectedBy: aSystemAnnouncement [

	^ aSystemAnnouncement affectsPackages
]
