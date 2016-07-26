tests
testSetForward
	"If the bug exist there will be an infinte reccursion."

	"self new testSetForward"

	"self run: #testSetForward"

	| t |
	cases := {(t := TransformationMorph new openCenteredInWorld)}.
	self
		shouldntTakeLong: [ 
			t forwardDirection: 180.0.
			self assert: t forwardDirection = 0.0 ].	"and without a rendee it should not change things."
	^ true