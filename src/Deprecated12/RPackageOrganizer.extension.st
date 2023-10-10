Extension { #name : 'RPackageOrganizer' }

{ #category : '*Deprecated12' }
RPackageOrganizer >> addCategory: aCategory [

	self deprecated: 'The manipulation of categories got deprecated in Pharo 12. It is better to manipulate directly the packages and tags.'.
	self ensurePackageMatching: aCategory
]

{ #category : '*Deprecated12' }
RPackageOrganizer >> categories [

	| categories |
	self deprecated: 'The manipulation of categories got deprecated in Pharo 12. It is better to manipulate directly the packages and tags.'.
	categories := Set new.
	self packages do: [ :p |
		categories add: p name.
		categories addAll: (p classTags collect: [ :tag | tag categoryName ]) ].

	^ categories asArray
]

{ #category : '*Deprecated12' }
RPackageOrganizer >> categoriesMatching: matchString [

	self deprecated: 'The manipulation of categories got deprecated in Pharo 12. It is better to manipulate directly the packages and tags.'.
	^ self categories select: [ :c | matchString match: c ]
]

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
RPackageOrganizer >> createPackageNamed: aString [

	self deprecated: 'Use #addPackage: instead.' transformWith: '`@rcv createPackageNamed: `@arg' -> '`@rcv addPackage: `@arg'.
	^ self addPackage: aString
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
RPackageOrganizer >> includesCategory: aString [
	"Tests if a category is already included."

	self deprecated: 'The manipulation of categories got deprecated in Pharo 12. It is better to manipulate directly the packages and tags.'.
	^ self categories includes: aString
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
RPackageOrganizer >> listAtCategoryNamed: categoryName [
	"Answer the array of elements associated with the name, categoryName."

	self deprecated: 'The manipulation of categories got deprecated in Pharo 12. It is better to manipulate directly the packages and tags.'.
	^ (self tagForCategory: categoryName) classNames
]

{ #category : '*Deprecated12' }
RPackageOrganizer >> packageExactlyMatchingExtensionName: anExtensionName [
	"only look for a package for which the name match 'anExtensionName', making no difference about case. Return nil if no package is found"

	self
		deprecated: 'Use #packageNamed:ifAbsent: instead.'
		transformWith: '`@rcv packageExactlyMatchingExtensionName: `@arg' -> '`@rcv packageNamed:ifAbsent: `@arg ifAbsent: [ nil ]'.
	^ self packageNamed: anExtensionName ifAbsent: [ nil ]
]

{ #category : '*Deprecated12' }
RPackageOrganizer >> registerPackage: aPackage [

	self deprecated: 'Use #addPackage: instead.' transformWith: '`@rcv registerPackage: `@arg' -> '`@rcv addPackage: `@arg'.
	^ self addPackage: aPackage
]

{ #category : '*Deprecated12' }
RPackageOrganizer >> registerPackageNamed: aString [

	self deprecated: 'Use #ensurePackage: instead.' transformWith: '`@rcv registerPackageNamed: `@arg' -> '`@rcv ensurePackage: `@arg'.
	^ self ensurePackage: aString
]

{ #category : '*Deprecated12' }
RPackageOrganizer >> removeElement: behaviorName [

	self deprecated: 'The manipulation of categories got deprecated in Pharo 12. It is better to manipulate directly the packages and tags.'.
	self error.
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
RPackageOrganizer >> unregisterPackageNamed: symbol [

	self deprecated: 'Use #unregisterPackage: instead.' transformWith: '`@rcv unregisterPackageNamed: `@arg' -> '`@rcv unregisterPackage: `@arg'.
	^ self unregisterPackage: symbol
]
