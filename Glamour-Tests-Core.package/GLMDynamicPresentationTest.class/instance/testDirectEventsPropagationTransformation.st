tests
testDirectEventsPropagationTransformation
	| dynamic textPresentation counter portOne portTwo |
	counter := 0.
	dynamic := GLMDynamicPresentation new.
	dynamic entity: '42'.
	dynamic display: [ :entity | textPresentation := GLMRubricTextPresentation new ].
	
	"Build the presentation"
	dynamic currentPresentation.
	
	textPresentation announcer when: GLMContextChanged do: [ :ann |
		counter := counter + 1 ].
	portOne := (GLMSimplePort new) name: #portOne; value: 1.
	portTwo := (GLMSimplePort new) name: #portTwo; value: 2.
	dynamic outerPortEvent: (GLMPortEvent on: portOne previouslyValued: portTwo).
	
	self assert: counter equals: 1.