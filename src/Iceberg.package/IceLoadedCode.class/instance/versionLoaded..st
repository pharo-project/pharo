events
versionLoaded: version
	self loadedVersions at: version package put: version.
	version beCurrent.

	"Reset reference commit so that it gets recomputed the next time it is needed"
	referenceCommit := nil.