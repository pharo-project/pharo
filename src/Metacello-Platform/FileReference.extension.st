Extension { #name : #FileReference }

{ #category : #'*Metacello-Platform' }
FileReference >> asRepositorySpecFor: aMetacelloMCProject [
  ^ aMetacelloMCProject repositorySpec
    description: self fullName;
    type: 'directory';
    yourself
]
