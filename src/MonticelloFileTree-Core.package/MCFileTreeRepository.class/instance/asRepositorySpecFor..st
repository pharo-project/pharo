accessing
asRepositorySpecFor: aMetacelloMCProject
    ^ aMetacelloMCProject repositorySpec
        description: self description;
        type: 'filetree';
        yourself