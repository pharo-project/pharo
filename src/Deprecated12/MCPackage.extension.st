Extension { #name : 'MCPackage' }

{ #category : '*Deprecated12' }
MCPackage >> correspondingRPackage [
	"Use #systemPackage instead"

	self deprecated: 'Use #systemPackage instead' transformWith: '`@rcv correspondingRPackage' -> '`@rcv systemPackage'.
	^ self systemPackage
]

{ #category : '*Deprecated12' }
MCPackage >> packageSet [

	self deprecated: 'I represent a unique package, thus I should not use a package set. Use #systemPackage instead.'.
	^ RPackageSet named: name
]
