Extension { #name : #String }

{ #category : #'*RPackage-Core' }
String >> asPackage [
	^ self asPackageIfAbsent: [ NotFound new messageText: 'There is no package named ', self; object: self; signal ]
]

{ #category : #'*RPackage-Core' }
String >> asPackageIfAbsent: aBlock [

	^ self packageOrganizer packageNamed: self ifAbsent: aBlock
]

{ #category : #'*RPackage-Core' }
String >> isCategoryOf: prefix [
	"This method is used for the MC synchronization of RPackage with categorises. Since we want to remove the system of category, this method should not be used and has a vocation to be removed from the system."

	| prefixSize catSize |
	catSize := self size.
	prefixSize := prefix size.
	catSize < prefixSize ifTrue: [ ^ false ].
	(self findString: prefix startingAt: 1 caseSensitive: false) = 1 ifFalse: [ ^ false ].
	^ (self at: prefix size + 1 ifAbsent: [ ^ true ]) = $-
]
