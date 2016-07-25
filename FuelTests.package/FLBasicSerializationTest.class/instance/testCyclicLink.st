tests-cycles
testCyclicLink

	| a materializedA |
	a := Link new.
	a nextLink: a.
	
	materializedA := self resultOfSerializeAndMaterialize: a.

	"We can't check using the equality of the links because hangs ad infinitum. So we only check that the structure is right."
	self assert: materializedA nextLink == materializedA