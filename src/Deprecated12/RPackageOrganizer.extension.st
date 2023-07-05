Extension { #name : #RPackageOrganizer }

{ #category : #'*Deprecated12' }
RPackageOrganizer >> includesPackage: aPackage [

	self deprecated: 'Use #hasPackage: instead.' transformWith: '`@rcv includesPackage: `@arg' -> '`@rcv hasPackage: `@arg'.
	^ self hasPackage: aPackage
]

{ #category : #'*Deprecated12' }
RPackageOrganizer >> includesPackageNamed: aSymbol [
	
	self deprecated: 'Use #hasPackage: instead.' transformWith: '`@rcv includesPackageNamed: `@arg' -> '`@rcv hasPackage: `@arg'.
	^ self hasPackage: aSymbol
]

{ #category : #'*Deprecated12' }
RPackageOrganizer >> packageExactlyMatchingExtensionName: anExtensionName [
	"only look for a package for which the name match 'anExtensionName', making no difference about case. Return nil if no package is found"

	self
		deprecated: 'Use #packageNamedIgnoreCase:ifAbsent: instead.'
		transformWith: '`@rcv packageExactlyMatchingExtensionName: `@arg' -> '`@rcv packageNamedIgnoreCase: `@arg ifAbsent: [ nil ]'.
	^ self packageNamedIgnoreCase: anExtensionName ifAbsent: [ nil ]
]
