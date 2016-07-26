tests
testPresentationTransformation
	| dynamic |
	dynamic := GLMDynamicPresentation new.
	dynamic entity: 42.
	dynamic presentationTransformation: [ :some :entity | (1 to: entity) do: [ :each | some text title: each asString ] ].
	self assert: dynamic currentPresentation presentations size equals: 42.
	self assert: dynamic currentPresentation pane == dynamic pane