tests
testValidateWithMultipleSelection
	| pane presentation |
	pane := GLMPane named: 'test'. 
	presentation := GLMListPresentation new.
	presentation beMultiple.
	presentation shouldValidate: true.
	pane addPresentation: presentation.
	presentation entity: ($a to: $d).
	self assert: (presentation validate: (Array with: $a) on: (pane port: #selection)).
	self assert: (presentation validate: (Array with: $a with: $b) on: (pane port: #selection)).
	self deny: (presentation validate: ($a) on: (pane port: #selection)).
	self deny: (presentation validate: (Array with: $z) on: (pane port: #selection)).