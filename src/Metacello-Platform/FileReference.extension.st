Extension { #name : #FileReference }

{ #category : #'*metacello-platform' }
FileReference >> asRepositorySpecFor: aMetacelloMCProject [
  ^ aMetacelloMCProject repositorySpec
    description: self fullName;
    type: 'directory';
    yourself
]
