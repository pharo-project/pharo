Extension { #name : 'RPackageOrganizer' }

{ #category : '*Deprecated12' }
RPackageOrganizer >> categoryOfElement: behaviorName [

	self deprecated: 'Use #categoryOfBehavior: instead' transformWith: '`@rcv categoryOfElement: `@arg' -> '`@rcv categoryOfBehavior: `@arg'.
	^ self categoryOfBehavior: behaviorName
]

{ #category : '*Deprecated12' }
RPackageOrganizer >> classesInCategory: category [

	self deprecated: 'The manipulation of categories got deprecated in Pharo 12. It is better to manipulate directly the packages and tags.'.
	(self tagForCategory: category) ifNotNil: [ :tag | ^ tag classes asArray ].

	^ #(  )
]

{ #category : '*Deprecated12' }
RPackageOrganizer >> ensureExistAndRegisterPackageNamed: aSymbol [

	self deprecated: 'Use #ensurePackage: instead.' transformWith: '`@rcv ensureExistAndRegisterPackageNamed: `@arg' -> '`@rcv ensurePackage: `@arg'.
	^ self ensurePackage: aSymbol
]

{ #category : '*Deprecated12' }
RPackageOrganizer >> globalPackageOf: aClass [

	self deprecated:
		'This method will be removed in the future version of Pharo because the implementation does not seems to match the old method comment and the usecases needing this method are too few.'.
	^ self packageOf: aClass
]

{ #category : '*Deprecated12' }
RPackageOrganizer >> includesPackage: aPackage [

	self deprecated: 'Use #hasPackage: instead.' transformWith: '`@rcv includesPackage: `@arg' -> '`@rcv hasPackage: `@arg'.
	^ self hasPackage: aPackage
]

{ #category : '*Deprecated12' }
RPackageOrganizer >> includesPackageNamed: aSymbol [

	self deprecated: 'Use #hasPackage: instead.' transformWith: '`@rcv includesPackageNamed: `@arg' -> '`@rcv hasPackage: `@arg'.
	^ self hasPackage: aSymbol
]

{ #category : '*Deprecated12' }
RPackageOrganizer >> isEmptyCategoryNamed: categoryName [

	self deprecated: 'The manipulation of categories got deprecated in Pharo 12. It is better to manipulate directly the packages and tags.'.
	^ (self tagForCategory: categoryName)
		  ifNil: [ #(  ) ]
		  ifNotNil: [ :tag | tag isEmpty ]
]

{ #category : '*Deprecated12' }
RPackageOrganizer >> packageExactlyMatchingExtensionName: anExtensionName [
	"only look for a package for which the name match 'anExtensionName', making no difference about case. Return nil if no package is found"

	self
		deprecated: 'Use #packageNamedIgnoreCase:ifAbsent: instead.'
		transformWith: '`@rcv packageExactlyMatchingExtensionName: `@arg' -> '`@rcv packageNamedIgnoreCase: `@arg ifAbsent: [ nil ]'.
	^ self packageNamedIgnoreCase: anExtensionName ifAbsent: [ nil ]
]

{ #category : '*Deprecated12' }
RPackageOrganizer >> registerPackageNamed: aString [

	self deprecated: 'Use #ensurePackage: instead.' transformWith: '`@rcv registerPackageNamed: `@arg' -> '`@rcv ensurePackage: `@arg'.
	^ self ensurePackage: aString
]

{ #category : '*Deprecated12' }
RPackageOrganizer >> removeElement: behaviorName [

	self deprecated: 'Use #removeBehavior: instead' transformWith: '`@rcv removeElement: `@arg' -> '`@rcv removeBehavior: `@arg'.
	^ self removeBehavior: behaviorName
]

{ #category : '*Deprecated12' }
RPackageOrganizer >> removeEmptyPackages [

	self deprecated: 'Use #removeEmptyPackagesAndTags instead' transformWith: '`@rcv removeEmptyPackages' -> '`@rcv removeEmptyPackagesAndTags'.
	self removeEmptyPackagesAndTags
]

{ #category : '*Deprecated12' }
RPackageOrganizer >> tagForCategory: category [

	(self packageMatchingExtensionName: category) ifNotNil: [ :package |
		package classTagNamed: (category withoutPrefix: package name , '-') ifPresent: [ :tag | ^ tag ] ].

	^ nil
]

{ #category : '*Deprecated12' }
RPackageOrganizer >> unregisterPackage: aPackage [

	self deprecated: 'Use #removePackage: instead.' transformWith: '`@rcv unregisterPackage: `@arg' -> '`@rcv removePackage: `@arg'.
	^ self removePackage: aPackage
]

{ #category : '*Deprecated12' }
RPackageOrganizer >> unregisterPackageNamed: symbol [

	self deprecated: 'Use #removePackage: instead.' transformWith: '`@rcv unregisterPackageNamed: `@arg' -> '`@rcv removePackage: `@arg'.
	^ self removePackage: symbol
]
