Class {
	#name : 'MetacelloFetchTarget',
	#superclass : 'MetacelloRecordTarget',
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

{ #category : 'accessing' }
MetacelloFetchTarget >> asEnsureTarget [
	"explicitly share the loaderPolicy"

	^ MetacelloFetchTarget new
		  forceFetch;
		  yourself
]

{ #category : 'actions' }
MetacelloFetchTarget >> fetchPackageFromSpec: packageSpec gofer: gofer withEngine: anEngine [

	| references nearestReference externalReference |
	references := self
		              retryingResolvePackageSpecReferences: packageSpec
		              gofer: gofer. "look up mcz file"
	nearestReference := references last
		                    asMetacelloCachingResolvedReference.

	externalReference := (references select: [ :ref |
		                      ref name = nearestReference name ]) first
		                     asMetacelloCachingResolvedReference.
	anEngine repositoryMap
		at: externalReference name
		put: externalReference repository.
	(self
		 resolveDependencies: externalReference
		 nearest: nearestReference
		 into: (OrderedCollection with: nearestReference)) do: [ :reference |
		| pSpec l mcVersion |
		mcVersion := reference version.
		(l := (GoferVersionReference name: reference name) resolveAllWith:
			      anEngine cacheGofer) isEmpty ifTrue: [
			anEngine cacheRepository storeVersion: mcVersion.
			reference == nearestReference
				ifTrue: [ pSpec := packageSpec ]
				ifFalse: [
					pSpec := packageSpec project packageSpec.
					pSpec name: mcVersion package name ] ] ].

	anEngine resetCacheGofer.
	^ externalReference
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
MetacelloFetchTarget >> linearLoadPackageSpec: packageSpec gofer: gofer withEngine: anEngine [

	| externalReference |
	super
		linearLoadPackageSpec: packageSpec
		gofer: gofer
		withEngine: anEngine.

	externalReference := self
		                     fetchPackageFromSpec: packageSpec
		                     gofer: gofer
		                     withEngine: anEngine.

	MetacelloNotification signal: 'Fetched -> ' , externalReference name " , ' --- '
		, externalReference repository repositoryDescription , ' --- '
		, externalReference repository description"
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
