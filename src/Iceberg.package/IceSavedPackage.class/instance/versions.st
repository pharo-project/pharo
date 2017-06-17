querying
versions
	"Return all versions of this package in the current branch, latest first. 
	Containing info in the repo makes it easier to refresh."
	^ self repository currentBranch versionsFor: self
