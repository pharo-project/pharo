accessing
indexOfAnyOf: aCollection ifAbsent: exceptionBlock
	"Answer the index of the first occurence of any element included in aCollection within the receiver.
	If the receiver does not contain anElement, answer the result of evaluating the argument, exceptionBlock."

	^self indexOfAnyOf: aCollection startingAt: 1 ifAbsent: exceptionBlock