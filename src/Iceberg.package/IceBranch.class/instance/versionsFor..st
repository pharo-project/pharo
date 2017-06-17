querying
versionsFor: package
	"Will only produce versions for commits that have actual changes for the package"
	^ self repository newCommitWalk 
		fromBranch: self;
		versionsFor: package