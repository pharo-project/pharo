"
I am abstract query of all packages from given scope.

I only implement method related to the retrieved items type.
So my subclasses still should implement main query methods
"
Class {
	#name : #ClyPackageQuery,
	#superclass : #ClyTypedQuery,
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #'items type' }
ClyPackageQuery class >> resultItemsType [
	^RPackage
]

{ #category : #execution }
ClyPackageQuery >> collectMetadataOf: aQueryResult by: anEnvironmentPlugin [
	anEnvironmentPlugin collectMetadataOfPackages: aQueryResult
]
