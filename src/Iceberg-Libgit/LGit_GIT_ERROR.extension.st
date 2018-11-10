Extension { #name : #'LGit_GIT_ERROR' }

{ #category : #'*Iceberg-Libgit' }
LGit_GIT_ERROR >> acceptError: aVisitor [
	^ aVisitor visitERROR: self
]
