Extension { #name : 'MetacelloMemberSpec' }

{ #category : '*Metacello-MC' }
MetacelloMemberSpec >> addToMetacelloPackages: aMetacelloPackagesSpec [

	aMetacelloPackagesSpec addMember: self
]

{ #category : '*Metacello-MC' }
MetacelloMemberSpec >> addToMetacelloRepositories: aMetacelloRepositoriesSpec [

	aMetacelloRepositoriesSpec addMember: self
]

{ #category : '*Metacello-MC' }
MetacelloMemberSpec >> mergeIntoMetacelloPackages: aMetacelloPackagesSpec [

	aMetacelloPackagesSpec addMember: self
]

{ #category : '*Metacello-MC' }
MetacelloMemberSpec >> mergeIntoMetacelloRepositories: aMetacelloRepositoriesSpec [

	aMetacelloRepositoriesSpec addMember: self
]

{ #category : '*Metacello-MC' }
MetacelloMemberSpec >> removeFromMetacelloPackages: aMetacelloPackagesSpec [

	aMetacelloPackagesSpec addMember: self
]

{ #category : '*Metacello-MC' }
MetacelloMemberSpec >> removeFromMetacelloRepositories: aMetacelloRepositoriesSpec [

	aMetacelloRepositoriesSpec addMember: self
]
