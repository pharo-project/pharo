versions
latestVersionMatching: versionPatternString includedBlessings: included excludedBlessings: excludedBlessings

	| excluded |
	excluded := excludedBlessings asSet copy.
	excluded removeAllFoundIn: included.
	^(self map values select: [:version |
		(included isEmpty or: [ included includes: version blessing ]) 
			and: [ (excluded includes: version blessing) not 
				and: [ version versionNumber match: versionPatternString ]]])
		detectMax: [:version | version ]