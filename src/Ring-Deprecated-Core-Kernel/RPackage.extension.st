Extension { #name : #RPackage }

{ #category : #'*Ring-Deprecated-Core-Kernel' }
RPackage >> asRingDefinition [
	^RGPackageDefinition named: self name asSymbol
]
