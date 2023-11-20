"
I am a query of all package tags from given scope.

Scope should support #packageTagsDo:. 
Currently it is only ClyPackageScope.

The scope delegates class group building to the environment plugins.
It asks each plugin to collect class group providers using: 

	plugin collectPackageTagsProviders 

And then each provider creates set of class groups using method: 

	groupProvider packageTagsIn: aPackageScope do: aBlockWithGroup
	
Look at ClyPackageTagProvider for details
"
Class {
	#name : 'ClyAllPackageTagsQuery',
	#superclass : 'ClyTypedQuery',
	#category : 'Calypso-SystemQueries-Queries',
	#package : 'Calypso-SystemQueries',
	#tag : 'Queries'
}

{ #category : 'items type' }
ClyAllPackageTagsQuery class >> resultItemsType [
	^ClyClassGroup
]

{ #category : 'execution' }
ClyAllPackageTagsQuery >> buildResult: aQueryResult [

	| groups |
	groups := OrderedCollection new.

	scope packageTagsDo: [ :each | groups add: each].

	aQueryResult fillWith: groups
]

{ #category : 'execution' }
ClyAllPackageTagsQuery >> checkEmptyResult [

	scope packageTagsDo: [ :each | ^false].

	^true
]

{ #category : 'execution' }
ClyAllPackageTagsQuery >> collectMetadataOf: aQueryResult by: anEnvironmentPlugin [

	
]

{ #category : 'printing' }
ClyAllPackageTagsQuery >> description [

	^'all class groups'
]

{ #category : 'system changes' }
ClyAllPackageTagsQuery >> isResult: aQueryResult affectedBy: aSystemAnnouncement [

	^scope includesPackageTagsAffectedBy: aSystemAnnouncement
]
