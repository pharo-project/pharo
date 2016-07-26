testing
imageIsClean
	| ancestors |
	ancestors := version workingCopy ancestors.
	^ ancestors size = 1
		and: [(ancestors first = self ancestorInfo)
				and: [self imagePatch isEmpty]]