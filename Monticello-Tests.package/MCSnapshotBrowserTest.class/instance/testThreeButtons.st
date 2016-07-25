testing
testThreeButtons
	self assertButtonExists: 'instance'.
	self assertButtonExists: '?'.
	self assertButtonExists: 'class'.