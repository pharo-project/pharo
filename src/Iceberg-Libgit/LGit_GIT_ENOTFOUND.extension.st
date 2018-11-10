Extension { #name : #'LGit_GIT_ENOTFOUND' }

{ #category : #'*Iceberg-Libgit' }
LGit_GIT_ENOTFOUND >> acceptError: aVisitor [
	^ aVisitor visitENOTFOUND: self
]
