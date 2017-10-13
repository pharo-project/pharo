accessing
asRepositorySpecFor: aMetacelloMCProject
  ^ aMetacelloMCProject repositorySpec
    description: self description;
    type: self class basicDescription;
    yourself