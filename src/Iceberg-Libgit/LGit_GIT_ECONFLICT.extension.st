Extension { #name : #'LGit_GIT_ECONFLICT' }

{ #category : #'*Iceberg-Libgit' }
LGit_GIT_ECONFLICT >> acceptError: aVisitor [
	^ aVisitor visitECONFLICT: self
]
