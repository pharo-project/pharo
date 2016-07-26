tests-cycles
testCyclicLinks

	| a b c materializedA |
	a := Link new.
	b := Link new.
	c := Link new.
	
	a nextLink: b.
	b nextLink: c.
	c nextLink: a.
	
	materializedA := self resultOfSerializeAndMaterialize: a.

	"We can't check using the equality of the links because hangs ad infinitum. So we only check that the structure is right."
	self assert: materializedA nextLink nextLink nextLink == materializedA.
	self deny: materializedA nextLink == materializedA.
	self deny: materializedA nextLink nextLink == materializedA.