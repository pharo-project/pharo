instance creation
from: myCommitish to: theirsCommitish
	^ self new 
		repository: myCommitish repository;
		myVersion: myCommitish;
		theirVersion: theirsCommitish;
		yourself.