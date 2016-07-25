tests
testSimplePresentations
	| pane presentation announcement |
	pane := GLMPane named: 'test'.
	presentation := GLMPresentation new.
	self assert: pane presentations isEmpty.
	announcement := nil.
	pane when: GLMPresentationsChanged do: [ :ann | announcement := ann ].
	pane addPresentation: presentation.
	self assert: pane presentations size equals: 1.
	self assert: pane presentations first == presentation.
	self assert: presentation pane == pane.
	self assert: announcement notNil