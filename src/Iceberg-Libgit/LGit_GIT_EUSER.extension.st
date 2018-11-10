Extension { #name : #'LGit_GIT_EUSER' }

{ #category : #'*Iceberg-Libgit' }
LGit_GIT_EUSER >> acceptError: aVisitor [
	^ aVisitor visitEUSER: self
]
