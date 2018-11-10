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
	#name : #ClyAllClassGroups,
	#superclass : #ClyTypedQuery,
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #'items type' }
ClyAllClassGroups class >> resultItemsType [
	^ClyClassGroup
]

{ #category : #execution }
ClyAllClassGroups >> buildResult: aQueryResult [ 

	| groups |
	groups := OrderedCollection new.
	
	scope classGroupsDo: [ :each | groups add: each].
	
	aQueryResult fillWith: groups
]

{ #category : #execution }
ClyAllClassGroups >> checkEmptyResult [

	scope classGroupsDo: [ :each | ^false].
	
	^true
]

{ #category : #execution }
ClyAllClassGroups >> collectMetadataOf: aQueryResult by: anEnvironmentPlugin [
	anEnvironmentPlugin collectMetadataOfClassGroups: aQueryResult 
]

{ #category : #printing }
ClyAllClassGroups >> description [

	^'all class groups'
]

{ #category : #'system changes' }
ClyAllClassGroups >> isResult: aQueryResult affectedBy: aSystemAnnouncement [
	
	^scope includesClassGroupsAffectedBy: aSystemAnnouncement
]
