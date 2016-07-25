accessing
cyclesWithAtLeast: aCollectionOfPDPackage
	^ cycles select: [ :cycle | cycle isAtLeastInCycle: aCollectionOfPDPackage ]