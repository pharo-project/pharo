walking
versionsFor: package detect: selectBlock ifNone: ifNoneBlock
	self modifyingPackage: package.
	self commitsDo: [ :commit | 
		| version | 
		version := commit versionFor: package.
		(selectBlock value: version) ifTrue: [ ^ version ]
	].

	^ ifNoneBlock value
  