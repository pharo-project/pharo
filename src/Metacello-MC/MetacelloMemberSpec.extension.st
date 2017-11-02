Extension { #name : #MetacelloMemberSpec }

{ #category : #'*metacello-mc' }
MetacelloMemberSpec >> addToMetacelloPackages: aMetacelloPackagesSpec [

	aMetacelloPackagesSpec addMember: self
]

{ #category : #'*metacello-mc' }
MetacelloMemberSpec >> addToMetacelloRepositories: aMetacelloRepositoriesSpec [

	aMetacelloRepositoriesSpec addMember: self
]

{ #category : #'*metacello-mc' }
MetacelloMemberSpec >> mergeIntoMetacelloPackages: aMetacelloPackagesSpec [

	aMetacelloPackagesSpec addMember: self
]

{ #category : #'*metacello-mc' }
MetacelloMemberSpec >> mergeIntoMetacelloRepositories: aMetacelloRepositoriesSpec [

	aMetacelloRepositoriesSpec addMember: self
]

{ #category : #'*metacello-mc' }
MetacelloMemberSpec >> removeFromMetacelloPackages: aMetacelloPackagesSpec [

	aMetacelloPackagesSpec addMember: self
]

{ #category : #'*metacello-mc' }
MetacelloMemberSpec >> removeFromMetacelloRepositories: aMetacelloRepositoriesSpec [

	aMetacelloRepositoriesSpec addMember: self
]
