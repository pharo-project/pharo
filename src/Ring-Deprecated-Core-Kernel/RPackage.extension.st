Extension { #name : #RPackage }

{ #category : #'*Ring-Deprecated-Core-Kernel' }
RPackage >> asRingDefinition [
	^RGPackage named: self name asSymbol
]
