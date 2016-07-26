tests
testCustomValidation
	| pane presentation |
	pane := GLMPane named: 'test'.
	presentation := GLMPresentation new.
	pane addPresentation: presentation.
	presentation customValidation: [ :object :aport :entity | aport ~= #selection or: [ object = (entity last + 40) ] ].
	presentation entity: (OrderedCollection with: 1 with: 2).
	self assert: presentation entity last equals: 2.
	self assert: (presentation validate: 42 on: (pane port: #selection))