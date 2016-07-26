convenience
denySearch: aString
	" if the new input is the same as before we do not expect a new search to be triggered . Therefore the context should not change "
	
	"self assert: (spotter currentStep context matches: aStringOrText)."
	
	self basicSearch: aString.
	
	" assert conditions after search "
	self assert: previousContext == currentContext.
	self assert: previousContext == spotter currentStep context.
	self assert: (previousContext matches: aString).