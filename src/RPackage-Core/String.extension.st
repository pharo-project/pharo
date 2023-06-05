Extension { #name : #String }

{ #category : #'*RPackage-Core' }
String >> asPackage [
	^ self asPackageIfAbsent: [ NotFound new messageText: 'There is no package named ', self; object: self; signal ]
]

{ #category : #'*RPackage-Core' }
String >> asPackageIfAbsent: aBlock [

	^ Smalltalk organization packageNamed: self ifAbsent: aBlock
]

{ #category : #'*RPackage-Core' }
String >> isCategoryOf: prefix [
	"This method is used for the MC synchronization of RPackage with categorises. Since we want to remove the system of category, this method should not be used and has a vocation to be removed from the system."

	^ self = prefix or: [ self beginsWith: prefix , '-' ]
]
