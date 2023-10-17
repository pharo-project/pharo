Extension { #name : 'MCHttpRepository' }

{ #category : '*Metacello-MC' }
MCHttpRepository >> asRepositorySpecFor: aMetacelloMCProject [

	^(aMetacelloMCProject repositorySpec)
		description:  self description;
	 	type: 'http';
		yourself
]

{ #category : '*Metacello-MC' }
MCHttpRepository class >> createRepositoryFromSpec: aRepositorySpec [

	^ self location: aRepositorySpec description user: aRepositorySpec username password: aRepositorySpec password
]

{ #category : '*Metacello-MC' }
MCHttpRepository class >> isAvailableFor: type [
	^ self = MCHttpRepository and: [ type = 'http' ]
]
