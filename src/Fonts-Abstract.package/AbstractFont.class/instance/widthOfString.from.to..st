measuring
widthOfString: aString from: firstIndex to: lastIndex 
	"Measure the length of the given string between start and stop index"
	| resultX |
	resultX := 0.
	firstIndex 
		to: lastIndex
		do: [ :i | resultX := resultX + (self widthOf: (aString at: i)) ].
	^ resultX