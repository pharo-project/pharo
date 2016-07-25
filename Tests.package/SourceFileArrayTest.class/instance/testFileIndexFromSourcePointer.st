testing
testFileIndexFromSourcePointer
	"Test derivation of file index for sources or changes file from source pointers"

	| sf |
	sf := SourceFileArray new.
	"sources file mapping"
	self assert: 1 = (sf fileIndexFromSourcePointer: 16r1000000).
	self assert: 1 = (sf fileIndexFromSourcePointer: 16r1000013).
	self assert: 1 = (sf fileIndexFromSourcePointer: 16r1FFFFFF).
	self assert: 1 = (sf fileIndexFromSourcePointer: 16r3000000).
	self assert: 1 = (sf fileIndexFromSourcePointer: 16r3000013).
	self assert: 1 = (sf fileIndexFromSourcePointer: 16r3FFFFFF).

	self assert: 1 = (sf fileIndexFromSourcePointer: 16r5000000).
	self assert: 1 = (sf fileIndexFromSourcePointer: 16r5000013).
	self assert: 1 = (sf fileIndexFromSourcePointer: 16r5FFFFFF).

	(16r1000000 to: 16r1FFFFFF by: 811) do: [:e | self assert: 1 = (sf fileIndexFromSourcePointer: e)].
	(16r3000000 to: 16r3FFFFFF by: 811) do: [:e | self assert: 1 = (sf fileIndexFromSourcePointer: e)].
	"changes file mapping"
	self assert: 2 = (sf fileIndexFromSourcePointer: 16r2000000).
	self assert: 2 = (sf fileIndexFromSourcePointer: 16r2000013).
	self assert: 2 = (sf fileIndexFromSourcePointer: 16r2FFFFFF).
	self assert: 2 = (sf fileIndexFromSourcePointer: 16r4000000).
	self assert: 2 = (sf fileIndexFromSourcePointer: 16r4000013).
	self assert: 2 = (sf fileIndexFromSourcePointer: 16r4FFFFFF).

	self assert: 2 = (sf fileIndexFromSourcePointer: 16r6000000).
	self assert: 2 = (sf fileIndexFromSourcePointer: 16r6000013).
	self assert: 2 = (sf fileIndexFromSourcePointer: 16r6FFFFFF).

	(16r2000000 to: 16r2FFFFFF by: 811) do: [:e | self assert: 2 = (sf fileIndexFromSourcePointer: e)].
	(16r4000000 to: 16r4FFFFFF by: 811) do: [:e | self assert: 2 = (sf fileIndexFromSourcePointer: e)].

	"the following numeric ranges are unused but currently produces results as follows"
	self assert: 0 = (sf fileIndexFromSourcePointer: 16r0000000).
	self assert: 0 = (sf fileIndexFromSourcePointer: 16r0000013).
	self assert: 0 = (sf fileIndexFromSourcePointer: 16r0FFFFFF)

