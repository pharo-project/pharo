Extension { #name : #FileReference }

{ #category : #'*Metacello-PharoCommonPlatform' }
FileReference >> asRepositorySpecFor: aMetacelloMCProject [
  ^ aMetacelloMCProject repositorySpec
    description: self fullName;
    type: 'directory';
    yourself
]
