as yet unclassified
testUpgrade
	
	| sv |
	sv := SystemVersion fromShortVersionString: 'Pharo1.1ALPHA'.
	self assert: sv version = 'Pharo1.1ALPHA'.
	sv upgrade.
	self assert: sv version = 'Pharo1.1ALPHA'
