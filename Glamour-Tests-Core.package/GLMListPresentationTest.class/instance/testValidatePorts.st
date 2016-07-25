tests
testValidatePorts
	| pane presentation |
	pane := GLMPane named: 'test'.
	presentation := GLMListPresentation new.
	pane addPresentation: presentation.
	presentation shouldValidate: true.
	presentation entity: #(1 2).
	presentation selection: 2.
	self assert: presentation selection equals: 2.
	presentation selection: 3.	"this should be invalid"
	self assert: presentation selection equals: 2