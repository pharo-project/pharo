*GT-InspectorExtensions-Core
on: aBlock
	"This utility method constructs a tally object and returns it without
	triggering the construction of the default report"
	^ MessageTally new
		spyEvery: 1
		on: aBlock;
		yourself 