Extension { #name : 'SUnitVisitor' }

{ #category : '*Deprecated14' }
SUnitVisitor >> visitRPackage: aPackage [

	self deprecated: 'Use #visitPackage: instead.' transformWith: '`@rcv visitRPackage: `@arg' -> '`@rcv visitPackage: `@arg'.
	^ self visitPackage: aPackage
]

{ #category : '*Deprecated14' }
SUnitVisitor >> visitRPackageSet: aRRPackageSet [
	"Defines the behaviour while visiting a RPackageSet.
	 The default implementation below is to visit all RPackages contained in the package set.
	"

	self deprecated: 'RPAckageSet are now removed from Pharo.'.
	^ aRRPackageSet packages collect: [ :package | package acceptSUnitVisitor: self ]
]
