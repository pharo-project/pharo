"
I am abstract query of all packages from given scope.

I only implement method related to the retrieved items type.
So my subclasses still should implement main query methods
"
Class {
	#name : 'ClyPackageQuery',
	#superclass : 'ClyTypedQuery',
	#category : 'Calypso-SystemQueries-Queries',
	#package : 'Calypso-SystemQueries',
	#tag : 'Queries'
}

{ #category : 'items type' }
ClyPackageQuery class >> resultItemsType [
	^Package
]

{ #category : 'execution' }
ClyPackageQuery >> collectMetadataOf: aQueryResult by: anEnvironmentPlugin [
	anEnvironmentPlugin collectMetadataOfPackages: aQueryResult
]
