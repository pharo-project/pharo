querying
incomingCommits
	"Returns only the commits modifying this package."
	^ self repository newCommitWalk 
		fromHead;
		uptoVersion: self; 
		versionsFor: self package
