accessing
version
	"self uniqueInstance version"

	| major minor rev |
	
	major := LGitWriteBackValueInt new.
	minor := LGitWriteBackValueInt new.
	rev := LGitWriteBackValueInt new.
	
	self libgit2_version: major minor: minor rev: rev.
	^Array with: major value with: minor value with: rev value