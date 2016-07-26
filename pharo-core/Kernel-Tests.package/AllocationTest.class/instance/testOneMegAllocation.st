testing
testOneMegAllocation
	"Documentating a weird bug in the allocator"
	
	| sz array failed |
	failed := false.
	sz := 1024*1024.
	array := [Array new: sz] on: OutOfMemory do: [:ex| failed := true].
	self assert: (failed or:[array size = sz]).
	