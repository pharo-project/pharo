testing
testOutOfMemorySignal
	"Ensure that OOM is signaled eventually"
	| sz |
	sz := 512*1024*1024. "work around the 1GB alloc bug"
	self should: [(1 to: 2000) collect: [:i| Array new: sz]] raise: OutOfMemory.

	"Call me when this test fails, I want your machine"
	self should:[Array new: SmallInteger maxVal] raise: OutOfMemory.
