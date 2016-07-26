printing
printShowingDecimalPlaces: placesDesired
	"This implementation avoids any rounding error caused by rounded or roundTo:"
	
	^self asTrueFraction printShowingDecimalPlaces: placesDesired