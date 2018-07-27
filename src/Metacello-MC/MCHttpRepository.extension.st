Extension { #name : #MCHttpRepository }

{ #category : #'*metacello-mc' }
MCHttpRepository >> asRepositorySpecFor: aMetacelloMCProject [

	^(aMetacelloMCProject repositorySpec)
		description:  self description;
	 	type: 'http';
		yourself
]

{ #category : #'*Metacello-MC' }
MCHttpRepository class >> createRepositoryFromSpec: aRepositorySpec on: aPlatform [
	^ aPlatform createHttpRepository: aRepositorySpec
]

{ #category : #'*Metacello-MC' }
MCHttpRepository class >> isAvailableFor: type [
	^ self = MCHttpRepository and: [ type = 'http' ]
]
