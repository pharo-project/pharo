tests
testHeading
	"If the bug exist there will be an infinte recursion."

	"self new testHeading"

	"self run: #testHeading"

	| t |
	cases := {(t := TransformationMorph new openCenteredInWorld)}.
	self
		shouldntTakeLong: [ 
			[ self assert: t heading = 0.0 ]
				ensure: [ t delete ] ].
	^ true