tests-cycles
testCyclicLinksInArray

	| a b c materializedA materialized |
	a := Link new.
	b := Link new.
	c := Link new.
	
	a nextLink: b.
	b nextLink: c.
	c nextLink: a.
	
	materialized := self resultOfSerializeAndMaterialize: (Array with: a with: b with: c).
	materializedA := materialized first.

	"We can't check using the equality of the links because hangs ad infinitum. So we only check that the structure is right."
	self assert: materialized size = 3.
	self assert: materializedA nextLink nextLink nextLink == materializedA.
	self deny: materializedA nextLink == materializedA.
	self deny: materializedA nextLink nextLink == materializedA.