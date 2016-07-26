repository handling
createRepository
	self repository: (IceMCGitRepository new
		localRepository: self location;
		remoteUrl: self origin url;
		subdirectory: (self subdirectory ifNil: '');
		readonly: false).
