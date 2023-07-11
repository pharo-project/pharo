Extension { #name : #RPackageOrganizer }

{ #category : #'*Deprecated12' }
RPackageOrganizer >> packageExactlyMatchingExtensionName: anExtensionName [
	"only look for a package for which the name match 'anExtensionName', making no difference about case. Return nil if no package is found"

	self
		deprecated: 'Use #packageNamedIgnoreCase:ifAbsent: instead.'
		transformWith: '`@rcv packageExactlyMatchingExtensionName: `@arg' -> '`@rcv packageNamedIgnoreCase: `@arg ifAbsent: [ nil ]'.
	^ self packageNamedIgnoreCase: anExtensionName ifAbsent: [ nil ]
]

{ #category : #'*Deprecated12' }
RPackageOrganizer >> registerPackageNamed: aString [

	self deprecated: 'Use #ensurePackage: instead.' transformWith: '`@rcv registerPackageNamed: `@arg' -> '`@rcv ensurePackage: `@arg'.
	^ self ensurePackage: aString
]
