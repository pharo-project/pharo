"
I return packages which extends methods available from given scope.

My scope should understand #methodsDo: which I use to filter all extensions and collect their extending packages.

I was mainly introduced to implement #extensions method group in full browser. When you expand it you see all packages which extends the class.
But I can give you extending package from any scope. For example you can retrieve all packages in the system which extends some class: 

	(ClyExtendingPackages from: ClyNavigationEnvironment currentImageScope) execute.

"
Class {
	#name : 'ClyExtendingPackagesQuery',
	#superclass : 'ClyPackageQuery',
	#category : 'Calypso-SystemQueries-Queries',
	#package : 'Calypso-SystemQueries',
	#tag : 'Queries'
}

{ #category : 'execution' }
ClyExtendingPackagesQuery >> buildResult: aQueryResult [

	| packages |
	packages := IdentitySet new.

	scope methodsDo: [ :method | method isExtension ifTrue: [ packages add: method package ] ].

	aQueryResult fillWith: packages
]

{ #category : 'execution' }
ClyExtendingPackagesQuery >> checkEmptyResult [

	scope methodsDo: [ :method | method isExtension ifTrue: [ ^ false ] ].

	^ true
]

{ #category : 'printing' }
ClyExtendingPackagesQuery >> description [

	^'extending packages'
]

{ #category : 'system changes' }
ClyExtendingPackagesQuery >> isResult: aQueryResult affectedBy: aSystemAnnouncement [

	^scope includesMethodsAffectedBy: aSystemAnnouncement
]
