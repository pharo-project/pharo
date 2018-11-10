Extension { #name : #LGitTag }

{ #category : #'*Iceberg-Adapters' }
LGitTag >> asIcebergObjectInRepository: backend [
	^ IceTag named: self name inRepository: backend
]
