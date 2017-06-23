*Iceberg-Adapters
commitIdFor: aMCWorkingCopy ifFound: aBlockClosure 
	(aMCWorkingCopy repositoryGroup includes: self)
		ifTrue: [ aBlockClosure value: self commitId ]