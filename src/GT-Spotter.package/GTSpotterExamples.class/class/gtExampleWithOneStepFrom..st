examples - basic
gtExampleWithOneStepFrom: aSpotter
	<gtExample>
	<label: 'Spotter object with one step'>
	<depends: #gtExampleBasic>
	| step |
	step := GTSpotterStep new spotter: aSpotter.
	aSpotter steps add: step.
	aSpotter currentStep: step.
	^ aSpotter