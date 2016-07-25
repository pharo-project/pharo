testing
cycleAlreadyFound: aPackageCycle
	^ cycles anySatisfy: [ :cycle | cycle = aPackageCycle ]