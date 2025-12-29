Extension { #name : 'FileReference' }

{ #category : '*Metacello-Core' }
FileReference >> asRepositorySpecFor: aMetacelloProject [

	^ aMetacelloProject repositorySpec
		  description: self fullName;
		  type: 'directory';
		  yourself
]
