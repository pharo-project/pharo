Extension { #name : 'String' }

{ #category : '*Kernel' }
String >> asPackage [
	^ self asPackageIfAbsent: [ NotFound new messageText: 'There is no package named ', self; object: self; signal ]
]

{ #category : '*Kernel' }
String >> asPackageIfAbsent: aBlock [

	^ self packageOrganizer packageNamed: self ifAbsent: aBlock
]
