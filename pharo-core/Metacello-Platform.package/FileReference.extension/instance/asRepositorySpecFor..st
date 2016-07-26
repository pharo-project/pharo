*metacello-platform
asRepositorySpecFor: aMetacelloMCProject
  ^ aMetacelloMCProject repositorySpec
    description: self fullName;
    type: 'directory';
    yourself