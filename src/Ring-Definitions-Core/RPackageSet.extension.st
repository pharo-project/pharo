Extension { #name : #RPackageSet }

{ #category : #'*Ring-Definitions-Core' }
RPackageSet >> changeRecordForOverriddenMethod: aMethodDefinition [
	^ SourceFiles
		changeRecordsFor: aMethodDefinition 
		detect: [ :protocol | 
			self 
				includesMethodCategory: protocol 
				ofClass: aMethodDefinition actualClass ]
]
