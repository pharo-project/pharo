tests
testTestTime
	"This is a control case. Should always pass. 
If it does not something external to the tests are slowing things down 
past the 1 millisecond mark."

	"self new testTestTime"

	"self run: #testTestTime"

	| t |
	cases := {(t := TransformationMorph new openCenteredInWorld)}.
	self shouldntTakeLong: [ self assert: true ].
	^ true