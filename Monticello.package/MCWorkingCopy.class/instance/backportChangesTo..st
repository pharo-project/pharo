operations
backportChangesTo: aVersionInfo
	| baseVersion fullPatch currentVersionInfo currentVersion newSnapshot newAncestry |
	currentVersionInfo := self currentVersionInfo.
	baseVersion := self repositoryGroup versionWithInfo: aVersionInfo.
	currentVersion := self repositoryGroup versionWithInfo: currentVersionInfo.
	fullPatch := currentVersion snapshot patchRelativeToBase: baseVersion snapshot.
	(MCChangeSelectionRequest new
		patch: fullPatch;
		label: 'Changes to Backport';
		signal ) ifNotNil:
		[:partialPatch |
		newSnapshot := MCPatcher apply: partialPatch to: baseVersion snapshot.
		newAncestry := MCWorkingAncestry new
							addAncestor: aVersionInfo;
							addStepChild: currentVersionInfo;
							yourself.
		MCPackageLoader updatePackage: package withSnapshot: newSnapshot.
		ancestry := newAncestry.
		self modified: false; modified: true]