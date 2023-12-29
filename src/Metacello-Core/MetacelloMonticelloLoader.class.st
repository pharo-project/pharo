Class {
	#name : 'MetacelloMonticelloLoader',
	#superclass : 'Object',
	#category : 'Metacello-Core-Gofer',
	#package : 'Metacello-Core',
	#tag : 'Gofer'
}

{ #category : 'loading' }
MetacelloMonticelloLoader >> fetchPackageDirective: aMetacelloPackageLoadDirective [

	| monticelloRepositories allResolvedReferences references nearestReference mcVersion |
	monticelloRepositories := self repositoriesFrom:
		                          aMetacelloPackageLoadDirective
			                          repositorySpecs.
	allResolvedReferences := monticelloRepositories flatCollect: [ :r |
		                         r goferReferences ].
	references := allResolvedReferences select: [ :each |
		              aMetacelloPackageLoadDirective spec name
		              = each packageName ].

	nearestReference := references last.
	mcVersion := nearestReference version.
	MCCacheRepository default storeVersion: mcVersion
]

{ #category : 'loading' }
MetacelloMonticelloLoader >> loadAtomicPackageDirectives: packageDirectives [
	"Each package directive can specify many repositorySpecs.
	Find the first one that defines the package (assumming they all are equivalent) and use that."

	| model references |
	model := MCVersionLoader new.

	references := packageDirectives collect: [ :e |
		| resolvedReference |
		resolvedReference := ((self repositoriesFrom: e repositorySpecs)
			flatCollect: [ :repo | | found |
				found := repo
					packageNamed: e spec name
					ifAbsent: [ nil ].
				found
					ifNil: [ #(  ) ]
					ifNotNil: [ { {found . repo} } ] ]) first.
		model addVersion: resolvedReference first.
		resolvedReference ].

	model load.

	packageDirectives do: [ :e |
		MetacelloNotification signal:
			'Loaded -> ' , e packageName , ' --- '
			, e repositorySpecs first description ]
]

{ #category : 'loading' }
MetacelloMonticelloLoader >> loadPackageDirective: aMetacelloPackageLoadDirective [

	self loadAtomicPackageDirectives: { aMetacelloPackageLoadDirective }
]

{ #category : 'repositories' }
MetacelloMonticelloLoader >> repositoriesFrom: aMetacelloMVRepositorySpecs [

	^ aMetacelloMVRepositorySpecs collect: [ :aSpec |
		  aSpec createRepository ]
]
