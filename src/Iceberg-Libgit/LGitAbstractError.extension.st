Extension { #name : #LGitAbstractError }

{ #category : #'*Iceberg-Libgit' }
LGitAbstractError >> acceptError: aVisitor [
	^ aVisitor visitGenericError: self
]
