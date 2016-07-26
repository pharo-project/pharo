tests
testAnnouncement
	| pane presentation announcement |
	pane := GLMPane named: 'test'.
	presentation := GLMPresentation new.
	pane addPresentation: presentation.
	announcement := nil.
	presentation when: GLMContextChanged do: [ :ann | announcement := ann ].
	(pane port: #entity) value: 42.
	self assert: announcement notNil.
	self assert: announcement property equals: #entity.
	self assert: announcement value equals: 42