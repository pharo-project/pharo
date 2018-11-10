Extension { #name : #LGitTreeEntry }

{ #category : #'*Iceberg-Adapters' }
LGitTreeEntry >> name [
	^ self filename
]

{ #category : #'*Iceberg-Adapters' }
LGitTreeEntry >> readStreamDo: aBlock [
	^ aBlock value: (ReadStream on: self object contents asString)
]

{ #category : #'*Iceberg-Adapters' }
LGitTreeEntry >> tree [
	^ self object
]
