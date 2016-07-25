tests
testClassReshapeShouldKeepClassBindingForTraitMethods
	"A method should have the same binding than its class even if it come from a trait"
	self reshapeClass.
	self assert: (self traitMethodBinding == class binding).
	
	