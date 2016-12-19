testing
testTwoSameGenerator
	| uuid1 uuid2 time1 time2 |
	uuid1 := generator next.
	uuid2 := generator next.
	"different UUIDs"
	self deny: uuid1 = uuid2.
	"same generator, same node id"
	self assert: uuid1 reverse readStream uint32 equals: uuid2 reverse readStream uint32.
	"same generator, consecutive counters (take roll-over into account)"
	self assert: ((uuid1 at: 10) + 1) \\ 256 equals: (uuid2 at: 10).
	"same generators, similar clock, within 100 milliseconds (stored time is in microseconds)"
	time1 := self timeFromUUID: uuid1.
	time2 := self timeFromUUID: uuid2.
	self assert: (time2 - time1) abs < (100 * 1000)