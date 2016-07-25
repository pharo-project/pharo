testing
testChangesFileAddressRange
	"Test file position to source pointer address translation for the changes file"
	
	| sf i p a a2 |
	sf := SourceFileArray new.
	0 to: 16r1FFFFFFF by: 4093 do: [:e |
		a := sf sourcePointerFromFileIndex: 2 andPosition: e.
		i := sf fileIndexFromSourcePointer: a.
		self assert: i = 2.
		p := sf filePositionFromSourcePointer: a.
		self assert: p = e.
		a2 := sf sourcePointerFromFileIndex: 2 andPosition: p.
		self assert: a2 = a].
	0 to: 16rFFFFFF by: 811 do: [:e |
		a := sf sourcePointerFromFileIndex: 2 andPosition: e.
		self assert: (a between: 16r2000000 and: 16r2FFFFFF)].
	16r1000000 to: 16r1FFFFFF by: 811 do: [:e |
		a := sf sourcePointerFromFileIndex: 2 andPosition: e.
		self assert: (a between: 16r4000000 and: 16r4FFFFFF)].
	16r2000000 to: 16r2FFFFFF by: 811 do: [:e |
		a := sf sourcePointerFromFileIndex: 2 andPosition: e.
		self assert: (a between: 16r6000000 and: 16r6FFFFFF)].
	16r3000000 to: 16r3FFFFFF by: 811 do: [:e |
		a := sf sourcePointerFromFileIndex: 2 andPosition: e.
		self assert: (a between: 16r8000000 and: 16r8FFFFFF)].
	16r4000000 to: 16r4FFFFFF by: 811 do: [:e |
		a := sf sourcePointerFromFileIndex: 2 andPosition: e.
		self assert: (a between: 16rA000000 and: 16rAFFFFFF)].
	16r5000000 to: 16r5FFFFFF by: 811 do: [:e |
		a := sf sourcePointerFromFileIndex: 2 andPosition: e.
		self assert: (a between: 16rC000000 and: 16rCFFFFFF)].
	16r6000000 to: 16r6FFFFFF by: 811 do: [:e |
		a := sf sourcePointerFromFileIndex: 2 andPosition: e.
		self assert: (a between: 16rE000000 and: 16rEFFFFFF)].
	16r7000000 to: 16r7FFFFFF by: 811 do: [:e |
		a := sf sourcePointerFromFileIndex: 2 andPosition: e.
		self assert: (a between: 16r10000000 and: 16r10FFFFFF)]



