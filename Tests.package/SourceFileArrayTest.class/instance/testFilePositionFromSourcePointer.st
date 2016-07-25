testing
testFilePositionFromSourcePointer
	"Test derivation of file position for sources or changes file from source pointers"

	| sf |
	sf := SourceFileArray new.
	"sources file"
	self assert: 0 = (sf filePositionFromSourcePointer: 16r1000000).
	self assert: 16r13 = (sf filePositionFromSourcePointer: 16r1000013).
	self assert: 16rFFFFFF = (sf filePositionFromSourcePointer: 16r1FFFFFF).
	self assert: 16r1000000 = (sf filePositionFromSourcePointer: 16r3000000).
	self assert: 16r1000013 = (sf filePositionFromSourcePointer: 16r3000013).
	self assert: 16r1FFFFFF = (sf filePositionFromSourcePointer: 16r3FFFFFF).
	"changes file"
	self assert: 0 = (sf filePositionFromSourcePointer: 16r2000000).
	self assert: 16r13 = (sf filePositionFromSourcePointer: 16r2000013).
	self assert: 16rFFFFFF = (sf filePositionFromSourcePointer: 16r2FFFFFF).
	self assert: 16r1000000 = (sf filePositionFromSourcePointer: 16r4000000).
	self assert: 16r1000013 = (sf filePositionFromSourcePointer: 16r4000013).
	self assert: 16r1FFFFFF = (sf filePositionFromSourcePointer: 16r4FFFFFF).
	"the following numeric ranges are unused but currently produces results as follows"
	self assert: 0 = (sf filePositionFromSourcePointer: 16r0000000).
	self assert: 16r13 = (sf filePositionFromSourcePointer: 16r0000013).
	self assert: 16rFFFFFF = (sf filePositionFromSourcePointer: 16r0FFFFFF)
