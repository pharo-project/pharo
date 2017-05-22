walking
versionsFor: package
	self modifyingPackage: package.
	^ self commits collect: [ :commit | commit versionFor: package ]
