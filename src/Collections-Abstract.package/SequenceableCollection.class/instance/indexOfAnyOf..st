accessing
indexOfAnyOf: aCollection
	"Answer the index of the first occurence of any element included in aCollection within the receiver.
	If the receiver does not contain anElement, answer zero, which is an invalid index."

	^self indexOfAnyOf: aCollection startingAt: 1 ifAbsent: [0]