testing
raiseDeprecationWarnings
	| oldRaiseWarning |
	oldRaiseWarning := Deprecation raiseWarning.
	[ Deprecation raiseWarning: true.
	self deprecatedMessage.
	self fail ]
		ensure: [ Deprecation raiseWarning: oldRaiseWarning ]