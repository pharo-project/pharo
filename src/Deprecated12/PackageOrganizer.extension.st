Extension { #name : 'PackageOrganizer' }

{ #category : '*Deprecated12' }
PackageOrganizer >> addCategory: aCategory [

	self deprecated: 'The manipulation of categories got deprecated in Pharo 12. It is better to manipulate directly the packages and tags.'.
	self ensurePackageMatching: aCategory
]

{ #category : '*Deprecated12' }
PackageOrganizer >> categories [

	| categories |
	self deprecated: 'The manipulation of categories got deprecated in Pharo 12. It is better to manipulate directly the packages and tags.'.
	categories := Set new.
	self packages do: [ :p |
		categories add: p name.
		categories addAll: (p tags collect: [ :tag | tag categoryName ]) ].

	^ categories asArray
]

{ #category : '*Deprecated12' }
PackageOrganizer >> categoriesMatching: matchString [

	self deprecated: 'The manipulation of categories got deprecated in Pharo 12. It is better to manipulate directly the packages and tags.'.
	^ self categories select: [ :c | matchString match: c ]
]

{ #category : '*Deprecated12' }
PackageOrganizer >> categoryOfElement: behaviorName [

	self deprecated: 'The concept of category go removed from Pharo.'.
	^ (self class environment at: behaviorName) category
]

{ #category : '*Deprecated12' }
PackageOrganizer >> classesInCategory: category [

	self deprecated: 'The manipulation of categories got deprecated in Pharo 12. It is better to manipulate directly the packages and tags.'.
	(self tagForCategory: category) ifNotNil: [ :tag | ^ tag classes asArray ].

	^ #(  )
]

{ #category : '*Deprecated12' }
PackageOrganizer >> createPackageNamed: aString [

	self deprecated: 'Use #addPackage: instead.' transformWith: '`@rcv createPackageNamed: `@arg' -> '`@rcv addPackage: `@arg'.
	^ self addPackage: aString
]

{ #category : '*Deprecated12' }
PackageOrganizer >> ensureExistAndRegisterPackageNamed: aSymbol [

	self deprecated: 'Use #ensurePackage: instead.' transformWith: '`@rcv ensureExistAndRegisterPackageNamed: `@arg' -> '`@rcv ensurePackage: `@arg'.
	^ self ensurePackage: aSymbol
]

{ #category : '*Deprecated12' }
PackageOrganizer >> globalPackageOf: aClass [

	self deprecated:
		'This method will be removed in the future version of Pharo because the implementation does not seems to match the old method comment and the usecases needing this method are too few.'.
	^ self packageOf: aClass
]

{ #category : '*Deprecated12' }
PackageOrganizer >> includesCategory: aString [
	"Tests if a category is already included."

	self deprecated: 'The manipulation of categories got deprecated in Pharo 12. It is better to manipulate directly the packages and tags.'.
	^ self categories includes: aString
]

{ #category : '*Deprecated12' }
PackageOrganizer >> includesPackage: aPackage [

	self deprecated: 'Use #hasPackage: instead.' transformWith: '`@rcv includesPackage: `@arg' -> '`@rcv hasPackage: `@arg'.
	^ self hasPackage: aPackage
]

{ #category : '*Deprecated12' }
PackageOrganizer >> includesPackageNamed: aSymbol [

	self deprecated: 'Use #hasPackage: instead.' transformWith: '`@rcv includesPackageNamed: `@arg' -> '`@rcv hasPackage: `@arg'.
	^ self hasPackage: aSymbol
]

{ #category : '*Deprecated12' }
PackageOrganizer >> isEmptyCategoryNamed: categoryName [

	self deprecated: 'The manipulation of categories got deprecated in Pharo 12. It is better to manipulate directly the packages and tags.'.
	^ (self tagForCategory: categoryName)
		  ifNil: [ #(  ) ]
		  ifNotNil: [ :tag | tag isEmpty ]
]

{ #category : '*Deprecated12' }
PackageOrganizer class >> isPackageDefinedForClass: aClass [

	self
		deprecated: 'This method is refering to a global which is not good. If you want to see if a class is packaged you should ask it to an instance of package organizer.'
		transformWith: '`@rcv isPackageDefinedForClass: `@class' -> '`@rcv packageOrganizer isClassInstalled: `@class'.

	^ self packageOrganizer isClassInstalled: aClass
]

{ #category : '*Deprecated12' }
PackageOrganizer >> listAtCategoryNamed: categoryName [
	"Answer the array of elements associated with the name, categoryName."

	self deprecated: 'The manipulation of categories got deprecated in Pharo 12. It is better to manipulate directly the packages and tags.'.
	^ (self tagForCategory: categoryName) classNames
]

{ #category : '*Deprecated12' }
PackageOrganizer >> packageExactlyMatchingExtensionName: anExtensionName [
	"only look for a package for which the name match 'anExtensionName', making no difference about case. Return nil if no package is found"

	self
		deprecated: 'Use #packageNamed:ifAbsent: instead.'
		transformWith: '`@rcv packageExactlyMatchingExtensionName: `@arg' -> '`@rcv packageNamed:ifAbsent: `@arg ifAbsent: [ nil ]'.
	^ self packageNamed: anExtensionName ifAbsent: [ nil ]
]

{ #category : '*Deprecated12' }
PackageOrganizer >> registerPackage: aPackage [

	self deprecated: 'Use #addPackage: instead.' transformWith: '`@rcv registerPackage: `@arg' -> '`@rcv addPackage: `@arg'.
	^ self addPackage: aPackage
]

{ #category : '*Deprecated12' }
PackageOrganizer >> registerPackageNamed: aString [

	self deprecated: 'Use #ensurePackage: instead.' transformWith: '`@rcv registerPackageNamed: `@arg' -> '`@rcv ensurePackage: `@arg'.
	^ self ensurePackage: aString
]

{ #category : '*Deprecated12' }
PackageOrganizer >> removeElement: behaviorName [

	self deprecated: 'The manipulation of categories got deprecated in Pharo 12. It is better to manipulate directly the packages and tags.'.
	self error.
]

{ #category : '*Deprecated12' }
PackageOrganizer >> removeEmptyPackages [

	self deprecated: 'Use #removeEmptyPackagesAndTags instead' transformWith: '`@rcv removeEmptyPackages' -> '`@rcv removeEmptyPackagesAndTags'.
	self removeEmptyPackagesAndTags
]

{ #category : '*Deprecated12' }
PackageOrganizer >> tagForCategory: category [

	(self packageMatchingExtensionName: category) ifNotNil: [ :package |
		package tagNamed: (category withoutPrefix: package name , '-') ifPresent: [ :tag | ^ tag ] ].

	^ nil
]

{ #category : '*Deprecated12' }
PackageOrganizer >> unregisterPackageNamed: symbol [

	self deprecated: 'Use #unregisterPackage: instead.' transformWith: '`@rcv unregisterPackageNamed: `@arg' -> '`@rcv unregisterPackage: `@arg'.
	^ self unregisterPackage: symbol
]
