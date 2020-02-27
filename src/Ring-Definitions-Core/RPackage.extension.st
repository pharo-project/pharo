Extension { #name : #RPackage }

{ #category : #'*Ring-Definitions-Core' }
RPackage >> asRingDefinition [
	^RGPackageDefinition named: self name asSymbol
]

{ #category : #'*Ring-Definitions-Core' }
RPackage >> changeRecordForOverriddenMethod: aMethodDefinition [
	^ SourceFiles
		changeRecordsFor: aMethodDefinition 
		detect: [ :protocol | 
			self 
				includesMethodCategory: protocol 
				ofClass: aMethodDefinition actualClass ]
]
