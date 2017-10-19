examples - basic
gtExampleWithOneStep
	<gtExample>
	<label: 'Spotter object with one step'>
	| step spotter |
	spotter := self gtExampleBasic.
	step := GTSpotterStep new spotter: spotter.
	spotter steps add: step.
	spotter currentStep: step.
	^ spotter