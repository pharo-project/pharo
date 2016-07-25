operations
// operand
 
 	"operand is a Duration or a Number"
 
 
 	^ operand isNumber
 		ifTrue: [ self class nanoSeconds: (self asNanoSeconds // operand) asInteger ]
 		ifFalse: [ self asNanoSeconds // operand asDuration asNanoSeconds ]
 