Class {
	#name : #MetacelloLoaderPolicy,
	#superclass : #Object,
	#instVars : [
		'overrideRepositories',
		'repositoryMap',
		'ensuredMap',
		'cacheRepository',
		'cacheGofer',
		'ignoreImage',
		'loadData',
		'loadDirective',
		'silently'
	],
	#category : #'Metacello-MC-Loaders'
}

{ #category : #'instance creation' }
MetacelloLoaderPolicy class >> overrideRepositories: aCollection [

	^self new overrideRepositories: aCollection
]

{ #category : #accessing }
MetacelloLoaderPolicy >> cacheGofer [

	cacheGofer == nil 
		ifTrue: [ 
			"don't use a caching Gofer here, since we expect the contents to change during a fetch operation"
			cacheGofer := Gofer new. 
			cacheGofer disablePackageCache.
			cacheGofer repository: self cacheRepository. ].
	^ cacheGofer
]

{ #category : #accessing }
MetacelloLoaderPolicy >> cacheRepository [

	cacheRepository == nil ifTrue: [ cacheRepository := MCDictionaryRepository new ].
	^ cacheRepository
]

{ #category : #accessing }
MetacelloLoaderPolicy >> cacheRepository: anMCRepository [

	cacheRepository := anMCRepository.
	"getting a new repository, so wipe out the cacheGofer and ensureMap"
	ensuredMap := cacheGofer := nil
]

{ #category : #accessing }
MetacelloLoaderPolicy >> ensuredMap [

	ensuredMap == nil ifTrue: [ ensuredMap := Dictionary new ].
	^ensuredMap
]

{ #category : #accessing }
MetacelloLoaderPolicy >> ensuredMap: anObject [
	ensuredMap := anObject
]

{ #category : #testing }
MetacelloLoaderPolicy >> hasRepositoryOverrides [

	^self overrideRepositories ~~ nil
]

{ #category : #accessing }
MetacelloLoaderPolicy >> ignoreImage [
	^ ignoreImage
]

{ #category : #accessing }
MetacelloLoaderPolicy >> ignoreImage: anObject [
	ignoreImage := anObject
]

{ #category : #initialization }
MetacelloLoaderPolicy >> initialize [

	self 
		repositoryMap; 
		cacheRepository;
		ensuredMap.
	ignoreImage := false
]

{ #category : #actions }
MetacelloLoaderPolicy >> load [

	overrideRepositories := Array with: self cacheRepository. "ensure that hasRepositoryOverrides is true"
	self loadDirective loadWithPolicy: self
]

{ #category : #accessing }
MetacelloLoaderPolicy >> loadData [

 	loadData == nil ifTrue: [ loadData := MetacelloLoadData new ].
	^loadData
]

{ #category : #accessing }
MetacelloLoaderPolicy >> loadDirective [

	loadDirective == nil ifTrue: [ loadDirective := MetacelloLinearLoadDirective new ].
	^ loadDirective
]

{ #category : #accessing }
MetacelloLoaderPolicy >> overrideRepositories [
	^ overrideRepositories
]

{ #category : #accessing }
MetacelloLoaderPolicy >> overrideRepositories: anObject [
	overrideRepositories := anObject
]

{ #category : #actions }
MetacelloLoaderPolicy >> pushAtomicLoadDirectivesDuring: aBlock for: aLoader [

	self pushLoadDirective: (MetacelloAtomicLoadDirective loader: aLoader) during: aBlock.
]

{ #category : #actions }
MetacelloLoaderPolicy >> pushExplicitLoadDirectivesDuring: aBlock for: aLoader [

	| directive |
	directive := MetacelloExplicitLoadDirective loader: aLoader.
	self pushLoadDirective: directive during: aBlock.
	^directive
]

{ #category : #actions }
MetacelloLoaderPolicy >> pushLinearLoadDirectivesDuring: aBlock for: aLoader [

	self pushLoadDirective: (MetacelloLinearLoadDirective loader: aLoader) during: aBlock.
]

{ #category : #private }
MetacelloLoaderPolicy >> pushLoadDirective: aLoaderDirective during: aBlock [

	|  oldRoot |
	self loadDirective add: aLoaderDirective.
	oldRoot := loadDirective.
	loadDirective := aLoaderDirective.
	aBlock ensure: [ loadDirective := oldRoot ].
]

{ #category : #accessing }
MetacelloLoaderPolicy >> repositoryMap [

	repositoryMap == nil ifTrue: [ repositoryMap := Dictionary new ].
	^repositoryMap
]

{ #category : #accessing }
MetacelloLoaderPolicy >> repositoryMap: anObject [
	repositoryMap := anObject
]

{ #category : #accessing }
MetacelloLoaderPolicy >> resetCacheGofer [

	cacheGofer := nil
]

{ #category : #accessing }
MetacelloLoaderPolicy >> silently [

	silently == nil ifTrue: [ silently := false ].
	^ silently
]

{ #category : #accessing }
MetacelloLoaderPolicy >> silently: anObject [
	silently := anObject
]
