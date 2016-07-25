tests
testSetup
	| presentation |
	presentation := GLMPresentation new. 
	self assert: presentation pane notNil. 
	self assert: (presentation pane presentations includes: presentation)