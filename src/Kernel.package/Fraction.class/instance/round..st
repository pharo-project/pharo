truncation and round off
round: numberOfWishedDecimal
	"Round the decimal part of the receiver to be limited to the number of wished decimal. Only leave a fixed amount of decimal"
   < expr: '(1/3 round: 2)' result: 0.33 >      
	< expr: '(111/100 round: 2)' result: 1.11 > 
	
	^self asFloat round: numberOfWishedDecimal