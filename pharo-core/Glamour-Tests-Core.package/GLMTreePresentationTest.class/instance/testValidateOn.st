tests
testValidateOn
	self assert: (presentation validate: #($e $f) on: (pane port: #selection)).
	self assert: (presentation validate: $a on: (pane port: #selection)).
	self deny: (presentation validate: $h on: (pane port: #selection))