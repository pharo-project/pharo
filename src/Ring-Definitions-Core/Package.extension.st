Extension { #name : 'Package' }

{ #category : '*Ring-Definitions-Core' }
Package >> asRingDefinition [
	^RGPackageDefinition named: self name asSymbol
]
