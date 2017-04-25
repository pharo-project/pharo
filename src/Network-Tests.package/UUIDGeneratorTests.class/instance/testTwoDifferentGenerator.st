testing
testTwoDifferentGenerator
	| generator1 generator2 uuid1 uuid2 time1 time2 |
	generator1 := UUIDGenerator new.
	generator2 := UUIDGenerator new.
	uuid1 := generator1 next.
	uuid2 := generator2 next.
	"different UUIDs"
	self deny: uuid1 = uuid2.
	"different generators, different node id"
	self deny: uuid1 reverse readStream uint32 = uuid2 reverse readStream uint32.
	"same generators, similar clock, within 100 milliseconds (stored time is in microseconds)"
	time1 := self timeFromUUID: uuid1.
	time2 := self timeFromUUID: uuid2.
	self assert: (time2 - time1) abs < (100 * 1000)