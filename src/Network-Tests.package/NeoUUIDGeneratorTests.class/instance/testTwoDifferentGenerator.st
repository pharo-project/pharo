testing
testTwoDifferentGenerator
	| generator1 generator2 uuid1 uuid2 time1 time2 |
	generator1 := NeoUUIDGenerator new.
	generator2 := NeoUUIDGenerator new.
	uuid1 := generator1 next.
	uuid2 := generator2 next.
	"different UUIDs"
	self deny: uuid1 = uuid2.
	"different genertors, different node id"
	self deny: uuid1 reverse readStream uint32 = uuid2 reverse readStream uint32.
	"different genertors, similar clock, within 100 milleseconds"
	time1 := self timeFromUUID: uuid1.
	time2 := self timeFromUUID: uuid2.
	self assert: (time2 - time1) abs < 100