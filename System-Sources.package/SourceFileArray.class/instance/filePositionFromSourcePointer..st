private - sourcepointer conversion
filePositionFromSourcePointer: anInteger 
	"Return the position of the source chunk addressed by anInteger"

	| hi lo |
	hi := anInteger // 33554432.
	lo := anInteger \\ 16777216.
	((anInteger bitAnd: 16777216) ~= 0
			or: [anInteger < 16777216 "compatibility with StandardSourceFileArray"])
		ifTrue: [^ hi * 16777216 + lo"sources file"]
		ifFalse: [^ hi - 1 * 16777216 + lo"changes file"]