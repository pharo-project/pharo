Class {
	#name : 'MetacelloLoaderPolicy',
	#superclass : 'Object',
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
	#category : 'Metacello-Core-Loaders',
	#package : 'Metacello-Core',
	#tag : 'Loaders'
}

{ #category : 'accessing' }
MetacelloLoaderPolicy >> cacheGofer [

	^ cacheGofer ifNil: [ "don't use a caching Gofer here, since we expect the contents to change during a fetch operation"
		  cacheGofer := Gofer new
			                disablePackageCache;
			                repository: self cacheRepository;
			                yourself ]
]

{ #category : 'accessing' }
MetacelloLoaderPolicy >> cacheRepository [

	^ cacheRepository ifNil: [ cacheRepository := MCDictionaryRepository new ]
]

{ #category : 'accessing' }
MetacelloLoaderPolicy >> cacheRepository: anMCRepository [

	cacheRepository := anMCRepository.
	"getting a new repository, so wipe out the cacheGofer and ensureMap"
	ensuredMap := cacheGofer := nil
]

{ #category : 'accessing' }
MetacelloLoaderPolicy >> ensuredMap [

	^ ensuredMap ifNil: [ ensuredMap := Dictionary new ]
]

{ #category : 'accessing' }
MetacelloLoaderPolicy >> ensuredMap: anObject [
	ensuredMap := anObject
]

{ #category : 'testing' }
MetacelloLoaderPolicy >> hasRepositoryOverrides [
"1halt."
	^ self overrideRepositories isNotNil
]

{ #category : 'accessing' }
MetacelloLoaderPolicy >> ignoreImage [
	^ ignoreImage
]

{ #category : 'accessing' }
MetacelloLoaderPolicy >> ignoreImage: anObject [
	ignoreImage := anObject
]

{ #category : 'initialization' }
MetacelloLoaderPolicy >> initialize [

	self 
		repositoryMap; 
		cacheRepository;
		ensuredMap.
	ignoreImage := false
]

{ #category : 'actions' }
MetacelloLoaderPolicy >> load [
	overrideRepositories := Array with: self cacheRepository. "ensure that hasRepositoryOverrides is true"
	self loadDirective loadWithPolicy: self
]

{ #category : 'accessing' }
MetacelloLoaderPolicy >> loadData [

	^ loadData ifNil: [ loadData := MetacelloLoadData new ]
]

{ #category : 'accessing' }
MetacelloLoaderPolicy >> loadDirective [

	^ loadDirective ifNil: [ loadDirective := MetacelloLinearLoadDirective new ]
]

{ #category : 'accessing' }
MetacelloLoaderPolicy >> overrideRepositories [
	^ overrideRepositories
]

{ #category : 'accessing' }
MetacelloLoaderPolicy >> overrideRepositories: anObject [
	overrideRepositories := anObject
]

{ #category : 'actions' }
MetacelloLoaderPolicy >> pushAtomicLoadDirectivesDuring: aBlock for: aLoader [

	self pushLoadDirective: (MetacelloAtomicLoadDirective loader: aLoader) during: aBlock.
]

{ #category : 'actions' }
MetacelloLoaderPolicy >> pushLinearLoadDirectivesDuring: aBlock for: aLoader [

	| directive |
	directive := MetacelloLinearLoadDirective loader: aLoader.
	self pushLoadDirective: directive during: aBlock.
	^ directive
]

{ #category : 'private' }
MetacelloLoaderPolicy >> pushLoadDirective: aLoaderDirective during: aBlock [

	|  oldRoot |
	self loadDirective add: aLoaderDirective.
	oldRoot := loadDirective.
	loadDirective := aLoaderDirective.
	aBlock ensure: [ loadDirective := oldRoot ].
]

{ #category : 'accessing' }
MetacelloLoaderPolicy >> repositoryMap [

	^ repositoryMap ifNil: [ repositoryMap := Dictionary new ]
]

{ #category : 'accessing' }
MetacelloLoaderPolicy >> repositoryMap: anObject [
	repositoryMap := anObject
]

{ #category : 'accessing' }
MetacelloLoaderPolicy >> resetCacheGofer [

	cacheGofer := nil
]

{ #category : 'accessing' }
MetacelloLoaderPolicy >> silently [

	^ silently ifNil: [ silently := false ]
]

{ #category : 'accessing' }
MetacelloLoaderPolicy >> silently: anObject [
	silently := anObject
]
