tests
testPreferenceFilesNotCollideWithRealPreferences
	"Ensure that setUp is done correctly and we do not override or change production preferences.
	See https://pharo.fogbugz.com/f/cases/19501"
	| strategy |
	strategy := identifier persistenceStrategy.
	[ strategy ] whileNotNil: [ 
		self deny: (FileLocator preferences contains: strategy preferences)
			description: 'Tested preference file should NOT be stored in the preferences directory.'.
		self assert: (FileLocator preferences contains: strategy defaultPreferences)
			description: 'Default preference file should BE stored in the preferences directory.'.
		strategy := strategy previous.
	].