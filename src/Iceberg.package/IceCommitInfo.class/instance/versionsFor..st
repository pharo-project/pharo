querying
versionsFor: package
	"Will only produce versions for commits that have actual changes for the package"
	^ self repository newCommitWalk 
		fromCommit: self;
		versionsFor: package