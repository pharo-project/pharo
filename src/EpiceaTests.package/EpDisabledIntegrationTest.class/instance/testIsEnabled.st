tests
testIsEnabled
	self deny: monitor isEnabled.
	monitor enable.
	self assert: monitor isEnabled.
	monitor disable.
	self deny: monitor isEnabled.
