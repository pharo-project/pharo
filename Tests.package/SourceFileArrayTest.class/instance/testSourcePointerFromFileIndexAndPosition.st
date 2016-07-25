testing
testSourcePointerFromFileIndexAndPosition
	"Test valid input ranges"

	| sf |
	sf := SourceFileArray new.
	self should: [ sf sourcePointerFromFileIndex: 0 andPosition: 0 ] raise: Error.
	sf sourcePointerFromFileIndex: 1 andPosition: 0.
	sf sourcePointerFromFileIndex: 2 andPosition: 0.
	self should: [ sf sourcePointerFromFileIndex: 0 andPosition: 3 ] raise: Error.
	self should: [ sf sourcePointerFromFileIndex: 1 andPosition: -1 ] raise: Error.
	sf sourcePointerFromFileIndex: 1 andPosition: 16r1FFFFFF.
	sf sourcePointerFromFileIndex: 1 andPosition: 16r2000000.
	self should: [ sf sourcePointerFromFileIndex: 3 andPosition: 0 ] raise: Error.
	self should: [ sf sourcePointerFromFileIndex: 4 andPosition: 0 ] raise: Error.
	self assert: 16r1000000 = (sf sourcePointerFromFileIndex: 1 andPosition: 0).
	self assert: 16r1000013 = (sf sourcePointerFromFileIndex: 1 andPosition: 16r13).
	self assert: 16r1FFFFFF = (sf sourcePointerFromFileIndex: 1 andPosition: 16rFFFFFF).
	self assert: 16r2000000 = (sf sourcePointerFromFileIndex: 2 andPosition: 0).
	self assert: 16r2000013 = (sf sourcePointerFromFileIndex: 2 andPosition: 16r13).
	self assert: 16r2FFFFFF = (sf sourcePointerFromFileIndex: 2 andPosition: 16rFFFFFF).
	self assert: 16r3000000 = (sf sourcePointerFromFileIndex: 1 andPosition: 16r1000000).
	self assert: 16r3000013 = (sf sourcePointerFromFileIndex: 1 andPosition: 16r1000013).
	self assert: 16r3FFFFFF = (sf sourcePointerFromFileIndex: 1 andPosition: 16r1FFFFFF).
	self assert: 16r4000000 = (sf sourcePointerFromFileIndex: 2 andPosition: 16r1000000).
	self assert: 16r4000013 = (sf sourcePointerFromFileIndex: 2 andPosition: 16r1000013).
	self assert: 16r4FFFFFF = (sf sourcePointerFromFileIndex: 2 andPosition: 16r1FFFFFF)