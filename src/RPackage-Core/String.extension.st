Extension { #name : #String }

{ #category : #'*RPackage-Core' }
String >> asPackage [
	^ self asPackageIfAbsent: [ NotFound new messageText: 'There is no package named ', self; object: self; signal ]
]

{ #category : #'*RPackage-Core' }
String >> asPackageIfAbsent: aBlock [

	^ Smalltalk organization packageNamed: self ifAbsent: aBlock
]
