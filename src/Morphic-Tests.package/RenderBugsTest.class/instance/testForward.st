tests
testForward
	"If the bug exist there will be an infinte recursion."

	"self new testForward"

	"self run: #testForward"

	| t |
	cases := {(t := TransformationMorph new openCenteredInWorld)}.
	self shouldntTakeLong: [ self assert: t forwardDirection = 0.0 ].
	^ true