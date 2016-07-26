bytecode generation
outOfRangeError: string index: index range: rangeStart to: rangeEnd
	"For now..."
	^self error: thisContext sender method selector, ' ', string
				, ' index ', index printString
				, ' is out of range ', rangeStart printString, ' to ', rangeEnd printString