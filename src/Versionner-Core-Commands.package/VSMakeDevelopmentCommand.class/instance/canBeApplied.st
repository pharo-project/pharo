testing
canBeApplied
	^ target isDevelopment not and: [ target isBaseline ]