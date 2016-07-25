tests
testIndirectEventsPropagationTransformation
	| dynamic textPresentation counter |
	counter := 0.
	dynamic := GLMDynamicPresentation new.
	dynamic entity: '42'.
	dynamic display: [ :entity | textPresentation := GLMRubricTextPresentation new ].
	"Build the presentation"
	dynamic currentPresentation.
	
	textPresentation announcer when: GLMContextChanged do: [ :ann |
		ann property = #selectionInterval
			ifTrue: [ counter := counter + 1 ] ].
	textPresentation selectionInterval: (1 to: 2).
	
	self assert: counter equals: 1.