testing
canBeUsed
	^ OSPlatform current isMacOS 
		and: [ self symbolFont isTTCFont 
		and: [ self symbolFont familyName = self symbolFont realFont face familyName ] ]