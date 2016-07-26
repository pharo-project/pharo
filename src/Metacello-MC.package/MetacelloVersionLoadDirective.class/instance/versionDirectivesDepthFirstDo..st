enumerating
versionDirectivesDepthFirstDo: aBlock

	self loadDirectives do: [:directive | directive versionDirectivesDepthFirstDo: aBlock ].
	aBlock value: self.