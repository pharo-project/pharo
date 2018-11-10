Extension { #name : #'LGit_GIT_EEOF' }

{ #category : #'*Iceberg-Libgit' }
LGit_GIT_EEOF >> acceptError: aVisitor [

	^ aVisitor visitEEOF: self
]
