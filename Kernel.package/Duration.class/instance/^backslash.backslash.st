operations
\\ operand
 
 	"modulo. Remainder defined in terms of //. Answer a Duration with the 
 	same sign as aDuration. operand is a Duration or a Number."
 
 	^ operand isNumber
 		ifTrue: [ self class nanoSeconds: (self asNanoSeconds \\ operand) ]
 		ifFalse: [ self - (operand * (self // operand)) ]
 