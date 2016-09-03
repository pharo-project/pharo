truncation and round off
round: numberOfWishedDecimal
	"Round the decimal part of the receiver to be limited to the number of wished decimal. Only leave a fixed amount of decimal"
   < expr: 10.12345 round: 2 result: 10.12 >
        
	| v |
	v := 10 raisedTo: numberOfWishedDecimal.
	^ ((self * v) rounded / v) asFloat
