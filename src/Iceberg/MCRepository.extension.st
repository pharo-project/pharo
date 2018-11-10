Extension { #name : #MCRepository }

{ #category : #'*Iceberg-Adapters' }
MCRepository >> commitIdFor: aMCWorkingCopy ifFound: aBlockClosure [ 
	"Do nothing. By default MCRepositories can not provide a commitId for you."
]
