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

{ #category : 'visiting' }
MetacelloFetchTarget >> visitLinearLoadDirective: aMetacelloLinearLoadDirective [

	aMetacelloLinearLoadDirective loadDirectives do: [ :dir |
		dir acceptVisitor: self ]
]

{ #category : 'visiting' }
MetacelloFetchTarget >> visitPackageLoadDirective: aMetacelloPackageLoadDirective [
	"Fetch the package!"

	| gofer references nearestReference externalReference |
	gofer := Gofer new.
	gofer repository:
		aMetacelloPackageLoadDirective externalReference repository.

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
