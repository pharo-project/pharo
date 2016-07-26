accessing
lastIndexOfAnyOf: aCollection startingAt: lastIndex ifAbsent: exceptionBlock
	"Answer the index of the last occurence of anElement within the  
	receiver. If the receiver does not contain anElement, answer the
	result of evaluating the argument, exceptionBlock."

	lastIndex to: 1 by: -1 do:
		[:index |
		(aCollection includes: (self at: index)) ifTrue: [^ index]].
	^ exceptionBlock value