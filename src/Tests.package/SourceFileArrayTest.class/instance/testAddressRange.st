testing
testAddressRange
	"Test source pointer to file position address translation across a wide address range"
	
	| sf i p a |
	sf := SourceFileArray new.
	16r1000000 to: 16r10000000 by: 4093 do: [:e |
		i := sf fileIndexFromSourcePointer: e.
		p := sf filePositionFromSourcePointer: e.
		a := sf sourcePointerFromFileIndex: i andPosition: p.
		self assert: a = e]
