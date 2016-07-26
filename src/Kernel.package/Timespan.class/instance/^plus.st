arithmetic
+ operand
	"operand conforms to protocol Duration" 	

	^ self class starting: (self start + operand) duration: self duration
