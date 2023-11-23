Extension { #name : 'MetacelloSpecTestCase' }

{ #category : '*Metacello-TestsMCCore' }
MetacelloSpecTestCase >> packageSpec [
    ^ self project packageSpec
]

{ #category : '*Metacello-TestsMCCore' }
MetacelloSpecTestCase >> repositoriesSpec [
    ^ self project repositoriesSpec
]

{ #category : '*Metacello-TestsMCCore' }
MetacelloSpecTestCase >> repositorySpec [
    ^ self project repositorySpec
]
