testing
testIgnoreDeprecationWarnings
	| oldRaiseWarning |
	oldRaiseWarning := Deprecation raiseWarning.
	[ Deprecation raiseWarning: false.
	self deprecatedMessage.
	self assert: true ]
		ensure: [ Deprecation raiseWarning: oldRaiseWarning ]