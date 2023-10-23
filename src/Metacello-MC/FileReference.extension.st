Extension { #name : 'FileReference' }

{ #category : '*Metacello-MC' }
FileReference >> asRepositorySpecFor: aMetacelloMCProject [
  ^ aMetacelloMCProject repositorySpec
    description: self fullName;
    type: 'directory';
    yourself
]
