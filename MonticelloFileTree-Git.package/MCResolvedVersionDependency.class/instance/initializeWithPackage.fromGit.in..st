initialization
initializeWithPackage: aPackage fromGit: aCommitID in: aRepository
	package := aPackage.
	version := aRepository versionFromPackageName: package name commit: aCommitID.
	versionInfo := version info