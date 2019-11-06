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

{ #category : #'*Ring-Definitions-Core' }
RPackage >> methodReferences [
	
	^ self methods collect: [:each | each methodReference] thenSelect: [:each| each isValid and: [each isLocalSelector]].
]
