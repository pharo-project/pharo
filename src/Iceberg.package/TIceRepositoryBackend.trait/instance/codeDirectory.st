requirements
codeDirectory
	"Code repository might have been deleted or image might have been moved."
	self repositoryDirectory exists
		ifTrue: [ ^ self basicCodeDirectory ]
		ifFalse: [ IceLocalRepositoryMissing signalFor: self frontend ]