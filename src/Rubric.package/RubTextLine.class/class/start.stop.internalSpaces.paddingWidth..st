instance creation
start: startInteger stop: stopInteger internalSpaces: spacesInteger paddingWidth: padWidthInteger
	"Answer an instance of me with the arguments as the start, stop points, 
	number of spaces in the line, and width of the padding."
	| line |
	line := self new firstIndex: startInteger lastIndex: stopInteger.
	^ line internalSpaces: spacesInteger paddingWidth: padWidthInteger