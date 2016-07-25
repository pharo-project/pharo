tests
warningTest

	self log: 'About to signal warning.'.
	Warning signal: 'Ouch'.
	self log: 'Warning signal handled and resumed.'