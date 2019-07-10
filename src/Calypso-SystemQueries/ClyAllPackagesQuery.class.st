"
I return all packages from the scope
"
Class {
	#name : #ClyAllPackagesQuery,
	#superclass : #ClyPackageQuery,
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #execution }
ClyAllPackagesQuery >> buildResult: aQueryResult [
	| packages |
	packages := OrderedCollection new.
	
	scope packagesDo: [ :each | packages add: each].
	
	aQueryResult fillWith: packages
]

{ #category : #execution }
ClyAllPackagesQuery >> checkEmptyResult [
	
	scope packagesDo: [ :each | ^false].
	
	^true
]

{ #category : #printing }
ClyAllPackagesQuery >> description [

	^'all packages'
]

{ #category : #'system changes' }
ClyAllPackagesQuery >> isResult: aQueryResult affectedBy: aSystemAnnouncement [
	"Normally system shows dirty state of package after any system change.
	It means that we think that any system change affects packages.
	But generally it should be responsibility of plugin to know
	that package items are marked by dirty tag.
	
	Also right now we don't care that there is possibility to query packages from other objects than from packages themselves or general system environment.
	So any package change is related to my items"
	
	^aSystemAnnouncement affectsPackages
]
