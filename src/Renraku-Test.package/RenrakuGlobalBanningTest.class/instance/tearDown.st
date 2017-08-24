initialization
tearDown
	oldRulesSettings keysAndValuesDo: [ :rule :setting |
		rule enabled: setting ].
	super tearDown