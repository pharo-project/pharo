"
I return all contents of the project.
The actual objects are depends on project implementation. Look at ClyManagedProject for details.

"
Class {
	#name : #ClyProjectContentQuery,
	#superclass : #ClyTypedQuery,
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #accessing }
ClyProjectContentQuery class >> resultItemsType [
	^ClyProjectChildItem
]

{ #category : #execution }
ClyProjectContentQuery >> buildResult: aQueryResult [

	| allItems |
	allItems := OrderedCollection new.
	
	scope projectsDo: [ :each | 
		allItems addAll: each items  ].
	
	aQueryResult fillWith: allItems
]

{ #category : #execution }
ClyProjectContentQuery >> checkEmptyResult [
	
	scope projectsDo: [ :each | 
		each items ifNotEmpty: [ ^false ]  ].
	^true
]

{ #category : #execution }
ClyProjectContentQuery >> collectMetadataOf: aQueryResult by: anEnvironmentPlugin [

]

{ #category : #'system changes' }
ClyProjectContentQuery >> isResult: aQueryResult affectedBy: aSystemAnnouncement [
	self flag: #todo.
	^false
]
