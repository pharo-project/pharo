Extension { #name : 'FileReference' }

{ #category : '*Metacello-Core' }
FileReference >> asRepositorySpecFor: aMetacelloMCProject [
  ^ aMetacelloMCProject repositorySpec
    description: self fullName;
    type: 'directory';
    yourself
]
