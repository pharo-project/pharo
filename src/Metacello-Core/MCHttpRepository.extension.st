Extension { #name : 'MCHttpRepository' }

{ #category : '*Metacello-Core' }
MCHttpRepository >> asRepositorySpecFor: aMetacelloMCProject [

	^(aMetacelloMCProject repositorySpec)
		description:  self description;
	 	type: 'http';
		yourself
]

{ #category : '*Metacello-Core' }
MCHttpRepository class >> createRepositoryFromSpec: aRepositorySpec [

	^ self location: aRepositorySpec description user: aRepositorySpec username password: aRepositorySpec password
]

{ #category : '*Metacello-Core' }
MCHttpRepository class >> isAvailableFor: type [
	^ self = MCHttpRepository and: [ type = 'http' ]
]
