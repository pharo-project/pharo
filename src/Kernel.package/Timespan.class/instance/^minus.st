arithmetic
- operand
	"operand conforms to protocol DateAndTime or protocol Duration"

	^ (operand respondsTo: #asDateAndTime) 
	 	ifTrue: [ self start - operand ]
	 	ifFalse: [ self + (operand negated) ]. 