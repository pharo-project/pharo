patches
updated
	| newVersion |
	newVersion := self package computeLoadedVersion.
	self package repository loadedCode versionLoaded: newVersion.
	^ newVersion