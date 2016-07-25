versions
latestVersion

	| excluded |
"	self deprecated: 'Please use #stableVersion instead.'.
"	self flag: 'deprecate after version 1.0'.
	excluded := self excludeFromLatestVersion.
	^(self map values select: [:version |  
		(excluded includes: version blessing) not ]) detectMax: [:version | version ]