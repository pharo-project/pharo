actions
canDiveIn: aCandidate
	| newFakeStep canDiveIn|
	canDiveIn := false.
	newFakeStep := self spotter newStep origin: self origin.
	self spotter
		addStep: newFakeStep
		during: [ aCandidate spotterProcessorsFor: newFakeStep.
			canDiveIn := newFakeStep processors notEmpty ].
	^ canDiveIn