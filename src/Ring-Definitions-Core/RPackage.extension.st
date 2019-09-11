Extension { #name : #RPackage }

{ #category : #'*Ring-Definitions-Core' }
RPackage >> asRingDefinition [
	^RGPackageDefinition named: self name asSymbol
]
