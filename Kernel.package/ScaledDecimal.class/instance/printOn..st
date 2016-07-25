printing
printOn: aStream
	"Append an approximated representation of the receiver on aStream.
	Use prescribed number of digits after decimal point (the scale) using a rounding operation if not exact"
	
	self printOn: aStream showingDecimalPlaces: scale.

	"Append a scale specification so that the number can be recognized as a ScaledDecimal"
	aStream nextPut: $s; print: scale.