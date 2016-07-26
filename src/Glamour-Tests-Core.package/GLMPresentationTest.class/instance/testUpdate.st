tests
testUpdate
	| col pane presentation |
	col := OrderedCollection with: 1 with: 2.
	pane := GLMPane named: 'test'.
	presentation := GLMListPresentation new.
	pane addPresentation: presentation.
	presentation entity: col.
	presentation shouldValidate: true.
	presentation update.
	presentation selection: 2.
	self assert: presentation selection equals: 2.
	col removeLast.
	presentation update.
	self assert: presentation selection isNil