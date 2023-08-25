"
I'm a set who can be instantiated by pattern matching RPackages (""name"" and ""name-*"").
My purpose is to provide a bridge between monticello and system packages, to allow backward compatibility with old packaging. 

"
Class {
	#name : #RPackageSet,
	#superclass : #Object,
	#instVars : [
		'packageName',
		'packages',
		'classes',
		'definedClasses',
		'extensionMethods',
		'methods'
	],
	#classInstVars : [
		'cacheActive',
		'cachePackages'
	],
	#category : #'RPackage-Core-Base'
}

{ #category : #private }
RPackageSet class >> basicNamed: aString [
	^ self basicNew
		initialize: aString;
		yourself
]

{ #category : #private }
RPackageSet class >> cacheActive: aBoolean [
	cacheActive := aBoolean
]

{ #category : #private }
RPackageSet class >> cachePackageAt: aString ifAbsentPut: aBlock [
	"self addCount: aString."
	^ self cachePackages
		at: aString asSymbol
		ifAbsentPut: aBlock
]

{ #category : #private }
RPackageSet class >> cachePackages [
	^ cachePackages ifNil: [ cachePackages := IdentityDictionary new ]
]

{ #category : #testing }
RPackageSet class >> isCacheActive [
	^ cacheActive ifNil: [ cacheActive := false ]
]

{ #category : #'instance creation' }
RPackageSet class >> named: aString [

	^ self isCacheActive
		ifTrue: [
			self
				cachePackageAt: aString
				ifAbsentPut: [ self basicNamed: aString ] ]
		ifFalse: [ self basicNamed: aString ]
]

{ #category : #private }
RPackageSet class >> resetCache [
	cachePackages := nil
]

{ #category : #accessing }
RPackageSet class >> withCacheDo: aBlock [
	| oldActive |

	oldActive := self isCacheActive.
	self cacheActive: true.
	aBlock ensure: [
		self cacheActive: oldActive.
		self isCacheActive ifFalse: [ self resetCache ] ]
]

{ #category : #'system compatibility' }
RPackageSet >> asRPackageSet [
	^self
]

{ #category : #accessing }
RPackageSet >> categoryNames [
	^  self packages
		inject:  #()
		into: [ :all :each | all, (each classTagCategoryNames asArray) ]
]

{ #category : #accessing }
RPackageSet >> classes [
	^classes ifNil: [ classes := self packages flatCollect: #definedClasses ]
]

{ #category : #accessing }
RPackageSet >> definedClasses [
	^definedClasses ifNil: [ definedClasses := self packages flatCollect: #definedClasses ]
]

{ #category : #'system compatibility' }
RPackageSet >> extendedClasses [
	^ self packages flatCollect: #extendedClasses
]

{ #category : #'system compatibility' }
RPackageSet >> extensionCategoriesForClass: aClass [

	self
		deprecated: 'Use #extensionProtocolsForClass: instead.'
		transformWith: '`@rcv extensionCategoriesForClass: `@arg' -> '`@rcv extensionProtocolsForClass: `@arg'.
	^ self extensionProtocolsForClass: aClass
]

{ #category : #accessing }
RPackageSet >> extensionMethods [
	^ extensionMethods ifNil: [
		extensionMethods :=
		(self packages flatCollect: [ :p | p extensionMethods ] as: Set) asArray ]
]

{ #category : #'system compatibility' }
RPackageSet >> extensionProtocolsForClass: aClass [

	^ aClass protocols select: [ :protocol | self isYourClassExtension: protocol ]
]

{ #category : #testing }
RPackageSet >> includesClass: aClass [
	^self packages anySatisfy: [ :each | each includesClass: aClass ]
]

{ #category : #'system compatibility' }
RPackageSet >> includesProtocol: protocol ofClass: aClass [

	^ (self isYourClassExtension: protocol) or: [ (self includesClass: aClass) and: [ (self isForeignClassExtension: protocol) not ] ]
]

{ #category : #'system compatibility' }
RPackageSet >> includesSystemCategory: categoryName [
	^self packages anySatisfy: [ :each |
		each includesSystemCategory: categoryName ]
]

{ #category : #initialization }
RPackageSet >> initialize: aString [

	self initialize.
	packageName := aString asSymbol.
	packages := (self organizer packageNamed: aString ifAbsent: [ nil ])
		            ifNotNil: [ :package | { package } ]
		            ifNil: [ {  } ]
]

{ #category : #'system compatibility' }
RPackageSet >> isForeignClassExtension: protocol [

	protocol ifNil: [ ^ false ].

	^ protocol isExtensionProtocol and: [ (self isYourClassExtension: protocol) not ]
]

{ #category : #'system compatibility' }
RPackageSet >> isYourClassExtension: protocol [

	protocol ifNil: [ ^ false ].

	^ protocol name asLowercase isCategoryOf: self methodCategoryPrefix
]

{ #category : #'system compatibility' }
RPackageSet >> methodCategoryPrefix [
	^ '*', self packageName asLowercase
]

{ #category : #accessing }
RPackageSet >> methods [
	^ methods ifNil: [ methods := (self packages flatCollect: [ :p | p methods ]) ]
]

{ #category : #querying }
RPackageSet >> organizer [

	(self packages isNil or: [ self package isEmpty ]) ifTrue: [ ^ self packageOrganizer ].

	^ self packages anyOne organizer
]

{ #category : #accessing }
RPackageSet >> packageName [
	^packageName
]

{ #category : #accessing }
RPackageSet >> packages [
	^packages
]

{ #category : #accessing }
RPackageSet >> unregister [
	self packages do: #unregister
]
