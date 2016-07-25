private - sourcepointer conversion
fileIndexFromSourcePointer: anInteger
	"Return the index of the source file which contains the source chunk addressed by anInteger"

	(anInteger bitAnd: 16r1000000) ~= 0
		ifTrue: [^1 "sources file"]
		ifFalse: [anInteger >= 16r1000000
			ifTrue: [^2 "changes file"]
			ifFalse: [^0 "compatibility with StandardSourceFileArray"]]