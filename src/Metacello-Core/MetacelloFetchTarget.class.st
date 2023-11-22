Class {
	#name : 'MetacelloFetchTarget',
	#superclass : 'MetacelloTarget',
	#instVars : [
		'shouldForceFetch'
	],
	#category : 'Metacello-Core-Loaders',
	#package : 'Metacello-Core',
	#tag : 'Loaders'
}

{ #category : 'accessing' }
MetacelloFetchTarget >> actionLabel [

	^'Fetching '
]

{ #category : 'versionInfo' }
MetacelloFetchTarget >> ancestorsFor: packageSpec [

	^self loadData
		ancestorsFor: packageSpec 
		ifAbsent: [ super ancestorsFor: packageSpec ]
]

{ #category : 'accessing' }
MetacelloFetchTarget >> asEnsureTarget [
	"explicitly share the loaderPolicy"

	^ (MetacelloFetchTarget on: self spec)
		  loaderPolicy: self loaderPolicy;
		  forceFetch;
		  yourself
]

{ #category : 'accessing' }
MetacelloFetchTarget >> asLoadingTarget [

	^(MetacelloLoadTarget on: self spec)
		loaderPolicy: self loaderPolicy copy;
		yourself
]

{ #category : 'private' }
MetacelloFetchTarget >> atomicLoadPackageOfSpec: aSpec [

	self loaderPolicy
		pushAtomicLoadDirectivesDuring: [
			super linearLoadPackagesOfSpec: aSpec ]
		for: self
]

{ #category : 'versionInfo' }
MetacelloFetchTarget >> currentVersionInfoFor: packageSpec [

	^self loadData
		currentVersionInfoFor: packageSpec 
		ifAbsent: [ super currentVersionInfoFor: packageSpec ]
]

{ #category : 'actions' }
MetacelloFetchTarget >> doLoad [

	self loaderPolicy copy load
]

{ #category : 'accessing' }
MetacelloFetchTarget >> ensuredMap [

	^self loaderPolicy ensuredMap
]

{ #category : 'initialization' }
MetacelloFetchTarget >> forceFetch [

	shouldForceFetch := true
]

{ #category : 'initialization' }
MetacelloFetchTarget >> initialize [

	super initialize.
	shouldForceFetch := false
]

{ #category : 'actions' }
MetacelloFetchTarget >> linearLoadPackageSpec: packageSpec gofer: gofer [

	MetacelloPlatform current
		do: [
			| references nearestReference cachedReference externalReference mcVersion loadedVersionInfos |
			cachedReference := nil.
			packageSpec searchCacheRepositoryForPackage: [ "check to see if mcz file is already in cacheRepository"
				cachedReference := self resolvePackageSpec: packageSpec cachedGofer: self loaderPolicy cacheGofer.
				(cachedReference isNotNil and: [ packageSpec getFile isNotNil ]) ifTrue: [
					cachedReference name = packageSpec file ifTrue: [ "exact match between packageSpec file and cache"
						^ self scheduleFetchFor: packageSpec cachedReference: cachedReference ] ] ].
			references := self retryingResolvePackageSpecReferences: packageSpec gofer: gofer. "look up mcz file"
			nearestReference := references last asMetacelloCachingResolvedReference.
			(cachedReference isNotNil and: [ cachedReference name = nearestReference name ]) ifTrue: [ "latest reference in repository matches cachedReference ... "
				^ self scheduleFetchFor: packageSpec nearestReference: nearestReference ].
			(self ignoreImage not and: [ (loadedVersionInfos := self ancestorsFor: packageSpec) isNotNil ]) ifTrue: [ "If the mcz is already loaded into the image, no need to copy"
				loadedVersionInfos do: [ :info |
					info name = nearestReference name ifTrue: [
						| spc |
						spc := packageSpec copy.
						spc file: info name.
						(MetacelloIgnorePackageLoaded signal: spc) ifFalse: [ ^ self ] ] ] ].
			externalReference := (references select: [ :ref | ref name = nearestReference name ]) first asMetacelloCachingResolvedReference.
			self repositoryMap at: externalReference name put: externalReference repository.
			(self resolveDependencies: externalReference nearest: nearestReference into: (OrderedCollection with: nearestReference)) do: [ :reference |
				| pSpec l |
				mcVersion := reference version.
				(l := (GoferVersionReference name: reference name) resolveAllWith: self loaderPolicy cacheGofer) isEmpty ifTrue: [
					self loaderPolicy cacheRepository storeVersion: mcVersion.
					reference == nearestReference
						ifTrue: [ pSpec := packageSpec ]
						ifFalse: [
							pSpec := packageSpec project packageSpec.
							pSpec name: mcVersion package name ].
					self loadData
						addVersion: mcVersion
						versionInfo: mcVersion info
						resolvedReference: reference
						packageSpec: pSpec ] ].
			self scheduleFetchFor: packageSpec externalReference: externalReference ]
		displaying: 'Fetching ' , packageSpec file
]

{ #category : 'private' }
MetacelloFetchTarget >> linearLoadPackagesOfSpec: aSpec [

	self loaderPolicy
		pushLinearLoadDirectivesDuring: [
			super linearLoadPackagesOfSpec: aSpec ]
		for: self
]

{ #category : 'accessing' }
MetacelloFetchTarget >> loadData [

	^self loaderPolicy loadData
]

{ #category : 'accessing' }
MetacelloFetchTarget >> loadDirective [

	^self loaderPolicy loadDirective
]

{ #category : 'doits' }
MetacelloFetchTarget >> postLoad: packageOrVersionSpec [

	(MetacelloDirective postLoadSpec: packageOrVersionSpec loader: self)  addTo: self loadDirective
]

{ #category : 'doits' }
MetacelloFetchTarget >> preLoad: packageOrVersionSpec [

	(MetacelloDirective preLoadSpec: packageOrVersionSpec loader: self) addTo: self loadDirective
]

{ #category : 'printing' }
MetacelloFetchTarget >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut: $(.
	self loadDirective printOn: aStream.
	aStream nextPut: $)
]

{ #category : 'private' }
MetacelloFetchTarget >> resolveDependencies: aResolvedReference nearest: nearestReference into: aCollection [

	| block retryCount coll notDone |
	self flag: 'Not used, but retained to avoid upgrade issues'.
	block := [ :dependency |
	         | reference |
	         reference := MetacelloCachingGoferResolvedReference name: dependency versionInfo name repository: aResolvedReference repository.
	         coll add: reference.
	         MetacelloNotification signal: 'Fetched dependency -> ' , reference name , ' --- ' , reference repository repositoryDescription.
	         self resolveDependencies: reference nearest: reference into: coll ].
	retryCount := 0.
	notDone := true.
	coll := OrderedCollection new.
	[ notDone and: [ retryCount < 3 ] ] whileTrue: [
		retryCount > 0 ifTrue: [ MetacelloNotification signal: '...RETRY' ].
		[ "ensure that all resolved references have cached their version while wrapped by error handler"
		aCollection do: [ :each | each version ].
		nearestReference version dependencies do: block.
		notDone := false ]
			on: Error
			do: [ :ex |
				retryCount := retryCount + 1.
				retryCount >= 3 ifTrue: [ ex pass ].
				coll := OrderedCollection new ] ].
	aCollection addAll: coll.
	^ aCollection
]

{ #category : 'private' }
MetacelloFetchTarget >> resolvePackageSpec: packageSpec cachedGofer: gofer [

	| versionReference references |
	versionReference := packageSpec goferLoaderReference.
	(references := versionReference resolveAllWith: gofer) isEmpty ifTrue: [ ^ nil ].
	^ references last asMetacelloCachingResolvedReference
]

{ #category : 'private' }
MetacelloFetchTarget >> scheduleFetchFor: packageSpec cachedReference: reference [
	"reference already in the cache during fetch ...schedule a load directive for reference, so ensured load will come from cache"

	shouldForceFetch ifFalse: [ ^ self ].
	^ self
		  scheduleFetchFor: packageSpec
		  reference: reference
		  message: 'Fetched -> (cached) ' , reference name , ' --- '
			  , reference repository repositoryDescription , ' --- '
			  , reference repository description
]

{ #category : 'private' }
MetacelloFetchTarget >> scheduleFetchFor: packageSpec externalReference: reference [

	^ self
		  scheduleFetchFor: packageSpec
		  reference: reference
		  message: 'Fetched -> ' , reference name , ' --- ' , reference repository repositoryDescription , ' --- ' , reference repository description
]

{ #category : 'private' }
MetacelloFetchTarget >> scheduleFetchFor: packageSpec nearestReference: reference [
	"latest version in repository already matches the cached reference...schedule a load directive for reference, so ensured load will come from cache"

	shouldForceFetch ifFalse: [ ^ self ].
	^ self
		  scheduleFetchFor: packageSpec
		  reference: reference
		  message: 'Fetched -> (nearest) ' , reference name , ' --- '
			  , reference repository repositoryDescription , ' --- '
			  , reference repository description
]

{ #category : 'private' }
MetacelloFetchTarget >> scheduleFetchFor: packageSpec reference: reference message: message [
	self loaderPolicy resetCacheGofer.
	self preLoad: packageSpec.
	(MetacelloDirective loadPackage: packageSpec externalReference: reference loader: self) addTo: self loadDirective.
	self postLoad: packageSpec.
	MetacelloNotification signal: message
]
