querying
incommingVersions
	^ self versions select: [ :version | version isNewerThan: self loadedVersion ]