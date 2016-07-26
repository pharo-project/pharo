arithmetic
= comparand 
 	"Answer whether the argument is a <Duration> representing the same 
 	period of time as the receiver."
 
 	^ self == comparand
 		ifTrue: [true]
 		ifFalse: 
 			[self species = comparand species 
 				ifTrue: [self asNanoSeconds = comparand asNanoSeconds]
 				ifFalse: [false] ]