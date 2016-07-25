tests
testClassReshapeShouldKeepClassBinding
	"Check that when a class is reshaped, the recompilation of the methods gives correct class bindings (in the literal array of the methods)"

	self reshapeClass.
	self assert: self methodBinding == class binding