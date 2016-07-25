accessing
versionWithInfo: aVersionInfo ifNone: aBlock
	self repositoriesDo: [:ea | (ea versionWithInfo: aVersionInfo) ifNotNil: [:v | ^ v]].
	^aBlock value