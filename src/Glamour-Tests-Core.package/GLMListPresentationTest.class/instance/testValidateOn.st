tests
testValidateOn
	| pane presentation |
	pane := GLMPane named: 'test'. 
	presentation := GLMListPresentation new.
	pane addPresentation: presentation.
	presentation shouldValidate: true.
	presentation entity: ($a to: $d).
	self deny: (presentation validate: (Array with: $a) on: (pane port: #selection)).
	self deny: (presentation validate: (Array with: $a with: $b) on: (pane port: #selection)).
	self assert: (presentation validate: $a on: (pane port: #selection)).
	self assert: (presentation validate: $b on: (pane port: #selection)).
	self deny: (presentation validate: $z on: (pane port: #selection)).