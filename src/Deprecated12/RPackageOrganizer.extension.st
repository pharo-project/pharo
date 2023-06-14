Extension { #name : #RPackageOrganizer }

{ #category : #'*Deprecated12' }
RPackageOrganizer >> packageExactlyMatchingExtensionName: anExtensionName [
	"only look for a package for which the name match 'anExtensionName', making no difference about case. Return nil if no package is found"

	self
		deprecated: 'Use #packageNamedIgnoreCase:ifAbsent: instead.'
		transformWith: '`@rcv packageExactlyMatchingExtensionName: `@arg' -> '`@rcv packageNamedIgnoreCase: `@arg ifAbsent: [ nil ]'.
	^ self packageNamedIgnoreCase: anExtensionName ifAbsent: [ nil ]
]
