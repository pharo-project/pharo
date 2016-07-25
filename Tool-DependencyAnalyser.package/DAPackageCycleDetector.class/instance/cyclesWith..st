accessing
cyclesWith: aPDPackage
	^ cycles select: [ :cycle |
			cycle isInCycle: aPDPackage
		 ]