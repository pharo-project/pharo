versions handling
versionLoaded: version 
	loadedCode versionLoaded: version.
	self announcer announce: (IcePackageLoaded version: version).
	self refresh
