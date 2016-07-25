private - sourcepointer conversion
sourcePointerFromFileIndex: index andPosition: position
	"Return a sourcePointer encoding the given file index and position"

	| hi lo |
	(index = 1 or: [index = 2])
		ifFalse: [self error: 'invalid source file index'].
	position < 0 ifTrue: [self error: 'invalid source code pointer'].
	hi := position // 16r1000000 *2 + index.
	lo := position \\ 16r1000000.
	^ hi * 16r1000000 + lo
