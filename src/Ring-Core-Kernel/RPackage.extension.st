Extension { #name : #RPackage }

{ #category : #'*Ring-Core-Kernel' }
RPackage >> asRingDefinition [
	^RGPackage named: self name asSymbol
]
