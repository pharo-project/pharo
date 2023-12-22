Class {
	#name : 'MetacelloFetchTarget',
	#superclass : 'MetacelloTarget',
	#category : 'Metacello-Core-Targets',
	#package : 'Metacello-Core',
	#tag : 'Targets'
}

{ #category : 'accessing' }
MetacelloFetchTarget >> actionLabel [

	^'Fetching '
]

{ #category : 'repositories' }
MetacelloFetchTarget >> repositoriesFrom: aMetacelloMVRepositorySpecs [

	| repositories repos |
	repositories := MCRepositoryGroup default repositories.
	repos := OrderedCollection new.
	aMetacelloMVRepositorySpecs do: [ :aSpec |
		| description |
		description := aSpec description.
		(repositories
			 detect: [ :rep | rep description = description ]
			 ifNone: [ aSpec createRepository ]) ifNotNil: [ :repo |
			repos add: repo ] ].
	^ repos
]

{ #category : 'private' }
MetacelloFetchTarget >> resolveDependencies: aResolvedReference nearest: nearestReference into: aCollection [

	| coll |
	self flag: 'Not used, but retained to avoid upgrade issues'.
	coll := OrderedCollection new.
	"ensure that all resolved references have cached their version while wrapped by error handler"
	aCollection do: [ :each | each version ].
	nearestReference version dependencies do: [ :dependency |
		| reference |
		reference := MetacelloCachingGoferResolvedReference
			             name: dependency versionInfo name
			             repository: aResolvedReference repository.
		coll add: reference.
		self resolveDependencies: reference nearest: reference into: coll ].
	aCollection addAll: coll.
	^ aCollection
]

{ #category : 'private' }
MetacelloFetchTarget >> resolvePackageSpecReferences: packageSpec gofer: gofer [

	| versionReference references localGofer |
	localGofer := gofer.
	packageSpec repositorySpecs notEmpty ifTrue: [
		localGofer := Gofer new.
		(self repositoriesFrom: packageSpec repositorySpecs) do: [ :repo |
			localGofer repository: repo ] ].
	packageSpec getFile ifNil: [ "don't use package-cache when trying to get latest version"
		localGofer disablePackageCache ].
	versionReference := packageSpec goferLoaderReference.
	references := versionReference resolveAllWith: localGofer.
	localGofer enablePackageCache.
	^ references
]

{ #category : 'visiting' }
MetacelloFetchTarget >> visitAtomicLoadDirective: aMetacelloAtomicLoadDirective [
	
	aMetacelloAtomicLoadDirective loadDirectives do: [ :dir |
		dir acceptVisitor: self ]
]

{ #category : 'visiting' }
MetacelloFetchTarget >> visitLinearLoadDirective: aMetacelloLinearLoadDirective [

	aMetacelloLinearLoadDirective loadDirectives do: [ :dir |
		dir acceptVisitor: self ]
]

{ #category : 'visiting' }
MetacelloFetchTarget >> visitPackageLoadDirective: aMetacelloPackageLoadDirective [
	"Fetch the package!"

	| gofer references nearestReference externalReference monticelloRepositories |
	gofer := Gofer new.
	monticelloRepositories := self repositoriesFrom:
		                          aMetacelloPackageLoadDirective
			                          externalReference.
	monticelloRepositories do: [ :repo | gofer repository: repo ].

	references := self
		              resolvePackageSpecReferences:
		              aMetacelloPackageLoadDirective spec
		              gofer: gofer. "look up mcz file"
	nearestReference := references last
		                    asMetacelloCachingResolvedReference.
	externalReference := (references select: [ :ref |
		                      ref name = nearestReference name ]) first
		                     asMetacelloCachingResolvedReference.
	engine repositoryMap
		at: externalReference name
		put: externalReference repository.
	(self
		 resolveDependencies: externalReference
		 nearest: nearestReference
		 into: (OrderedCollection with: nearestReference)) do: [ :reference |
		| mcVersion |
		mcVersion := reference version.
		((GoferVersionReference name: reference name) resolveAllWith:
			 engine cacheGofer) isEmpty ifTrue: [
			engine cacheRepository storeVersion: mcVersion ] ].
	engine resetCacheGofer
]

{ #category : 'visiting' }
MetacelloFetchTarget >> visitPostLoadDirective: aMetacelloPostLoadDirective [ 
	
	"Nothing"
]

{ #category : 'visiting' }
MetacelloFetchTarget >> visitPreLoadDirective: aMetacelloPostLoadDirective [ 
	
	"Nothing"
]
