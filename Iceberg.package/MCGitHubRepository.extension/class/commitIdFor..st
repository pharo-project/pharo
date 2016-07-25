*Iceberg-Adapters
commitIdFor: aWorkingCopy
	^ aWorkingCopy repositoryGroup repositories 
		detect: [ :repo | repo isKindOf: self ] 
		ifFound: #commitId
		ifNone: nil
