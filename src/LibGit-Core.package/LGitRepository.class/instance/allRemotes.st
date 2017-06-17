accessing
allRemotes
	| out |
	
	out := LGitStringArray externalNew autoRelease.
	self remote_list: out.
	^ out strings collect: [ :each | (LGitRemote of: self named: each) lookup ]