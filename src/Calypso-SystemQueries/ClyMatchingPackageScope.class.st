"
I am a subclass of scope of packages for matching patterns
"
Class {
	#name : 'ClyMatchingPackageScope',
	#superclass : 'ClyPackageScope',
	#instVars : [
		'prefix'
	],
	#category : 'Calypso-SystemQueries-Scopes',
	#package : 'Calypso-SystemQueries',
	#tag : 'Scopes'
}

{ #category : 'accessing' }
ClyMatchingPackageScope class >> defaultName [

	^ 'matching packages'
]

{ #category : 'initialization' }
ClyMatchingPackageScope class >> ofAll: basisObjects in: aNavigationEnvironment withName: aName [

	^ (self ofAll: basisObjects withName: aName) bindTo: aNavigationEnvironment
]

{ #category : 'initialization' }
ClyMatchingPackageScope class >> ofAll: basisObjects withName: aName [

	^self new
		basisObjects: basisObjects;
		prefix: aName 
]

{ #category : 'comparing' }
ClyMatchingPackageScope >> = anObject [

	^ super = anObject and: [ prefix = anObject prefix ]
]

{ #category : 'comparing' }
ClyMatchingPackageScope >> hash [

	^ super hash bitXor: prefix hash
]

{ #category : 'accessing' }
ClyMatchingPackageScope >> prefix [

	^ prefix
]

{ #category : 'accessing' }
ClyMatchingPackageScope >> prefix: aName [

	prefix := aName
]

{ #category : 'printing' }
ClyMatchingPackageScope >> printBasisOn: aStream [

	basisObjects ifEmpty: [ ^ self ].
	aStream nextPutAll: prefix , '* (...)'
]

{ #category : 'filtering' }
ClyMatchingPackageScope >> splitOnMatchingScopes [

	| packages allPrefixes |
	packages := PackageOrganizer default packages.

	allPrefixes := basisObjects flatCollect: [ :pkg |
			               | substrings partialName |
			               substrings := pkg name substrings: '-'.
			               partialName := ''.
			               (1 to: substrings size - 1) collect: [ :i |
					               partialName := partialName , (substrings at: i) , '-'.
					               partialName ] ].
	^ allPrefixes asSet collect: [ :pre |
		  self class ofAll: (packages select: [ :each | each name beginsWith: pre ]) in: environment withName: pre ]
]
