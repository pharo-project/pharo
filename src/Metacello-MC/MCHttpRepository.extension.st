Extension { #name : #MCHttpRepository }

{ #category : #'*metacello-mc' }
MCHttpRepository >> asRepositorySpecFor: aMetacelloMCProject [

	^(aMetacelloMCProject repositorySpec)
		description:  self description;
	 	type: 'http';
		yourself
]
