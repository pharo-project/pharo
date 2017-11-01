Extension { #name : #MetacelloSpecTestCase }

{ #category : #'*metacello-testsmccore' }
MetacelloSpecTestCase >> packageSpec [
    ^ self project packageSpec
]

{ #category : #'*metacello-testsmccore' }
MetacelloSpecTestCase >> repositoriesSpec [
    ^ self project repositoriesSpec
]

{ #category : #'*metacello-testsmccore' }
MetacelloSpecTestCase >> repositorySpec [
    ^ self project repositorySpec
]
