"
I am a query of all class groups from given scope.

Scope should support #classGroupsDo:. 
Currently it is only ClyPackageScope.

The scope delegates class group building to the environment plugins.
It asks each plugin to collect class group providers using: 

	plugin collectClassGroupProviders 

And then each provider creates set of class groups using method: 

	groupProvider classGroupsIn: aPackageScope do: aBlockWithGroup
	
Look at ClyClassGroupProvider for details
"
Class {
	#name : #ClyAllClassGroupsQuery,
	#superclass : #ClyTypedQuery,
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #'items type' }
ClyAllClassGroupsQuery class >> resultItemsType [
	^ClyClassGroup
]

{ #category : #execution }
ClyAllClassGroupsQuery >> buildResult: aQueryResult [ 

	| groups |
	groups := OrderedCollection new.
	
	scope classGroupsDo: [ :each | groups add: each].
	
	aQueryResult fillWith: groups
]

{ #category : #execution }
ClyAllClassGroupsQuery >> checkEmptyResult [

	scope classGroupsDo: [ :each | ^false].
	
	^true
]

{ #category : #execution }
ClyAllClassGroupsQuery >> collectMetadataOf: aQueryResult by: anEnvironmentPlugin [
	anEnvironmentPlugin collectMetadataOfClassGroups: aQueryResult 
]

{ #category : #printing }
ClyAllClassGroupsQuery >> description [

	^'all class groups'
]

{ #category : #'system changes' }
ClyAllClassGroupsQuery >> isResult: aQueryResult affectedBy: aSystemAnnouncement [
	
	^scope includesClassGroupsAffectedBy: aSystemAnnouncement
]
