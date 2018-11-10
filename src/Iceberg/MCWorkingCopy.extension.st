Extension { #name : #MCWorkingCopy }

{ #category : #'*Iceberg' }
MCWorkingCopy >> ancestry: anAncestry [

	ancestry := anAncestry
]

{ #category : #'*Iceberg' }
MCWorkingCopy >> info [
	^ self versionInfo
]

{ #category : #'*Iceberg-Adapters' }
MCWorkingCopy >> loadedVersion [
	self ancestors do: [ :ancestor |
		(self repositoryGroup 
			versionWithInfo: ancestor
			ifNone: [ MCRepositoryGroup default versionWithInfo: ancestor ifNone: [] ])
			ifNotNil: [ :version | ^ version ]
	].

]
