testing
testSourcesFileAddressRange
	"Test file position to source pointer address translation for the sources file"
	
	| sf i p a a2 |
	sf := SourceFileArray new.
	0 to: 16r1FFFFFFF by: 4093 do: [:e |
		a := sf sourcePointerFromFileIndex: 1 andPosition: e.
		i := sf fileIndexFromSourcePointer: a.
		self assert: i = 1.
		p := sf filePositionFromSourcePointer: a.
		self assert: p = e.
		a2 := sf sourcePointerFromFileIndex: 1 andPosition: p.
		self assert: a2 = a].
	0 to: 16rFFFFFF by: 811 do: [:e |
		a := sf sourcePointerFromFileIndex: 1 andPosition: e.
		self assert: (a between: 16r1000000 and: 16r1FFFFFF)].
	16r1000000 to: 16r1FFFFFF by: 811 do: [:e |
		a := sf sourcePointerFromFileIndex: 1 andPosition: e.
		self assert: (a between: 16r3000000 and: 16r3FFFFFF)].

	16r2000000 to: 16r2FFFFFF by: 811 do: [:e |
		a := sf sourcePointerFromFileIndex: 1 andPosition: e.
		self assert: (a between: 16r5000000 and: 16r5FFFFFF)].
	16r3000000 to: 16r3FFFFFF by: 811 do: [:e |
		a := sf sourcePointerFromFileIndex: 1 andPosition: e.
		self assert: (a between: 16r7000000 and: 16r7FFFFFF)].
	16r4000000 to: 16r4FFFFFF by: 811 do: [:e |
		a := sf sourcePointerFromFileIndex: 1 andPosition: e.
		self assert: (a between: 16r9000000 and: 16r9FFFFFF)].
	16r5000000 to: 16r5FFFFFF by: 811 do: [:e |
		a := sf sourcePointerFromFileIndex: 1 andPosition: e.
		self assert: (a between: 16rB000000 and: 16rBFFFFFF)].
	16r6000000 to: 16r6FFFFFF by: 811 do: [:e |
		a := sf sourcePointerFromFileIndex: 1 andPosition: e.
		self assert: (a between: 16rD000000 and: 16rDFFFFFF)].
	16r7000000 to: 16r7FFFFFF by: 811 do: [:e |
		a := sf sourcePointerFromFileIndex: 1 andPosition: e.
		self assert: (a between: 16rF000000 and: 16rFFFFFFF)]
