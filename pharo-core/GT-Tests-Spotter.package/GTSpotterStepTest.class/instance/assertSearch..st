convenience
assertSearch: aString

	" assert conditions before search "
	self deny: (spotter currentStep context matches: aString).
	
	self basicSearch: aString.
		
	" assert conditions after search "
	self deny: previousContext == currentContext.
	self deny: previousContext == spotter currentStep context.
	self assert: spotter currentStep context text asString = aString asString