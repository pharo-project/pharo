as yet unclassified
testUpgradeState
	
	| sv |
	sv := (SystemVersion fromShortVersionString: 'Pharo1.1ALPHA') upgrade.
	self assert: sv type = 'Pharo'.
	self assert: sv major = 1.
	self assert: sv minor = 1.
	self assert: sv suffix = 'ALPHA'.